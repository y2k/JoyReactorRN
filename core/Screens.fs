namespace JoyReactor.Components

module S = JoyReactor.SyncBuilder
module A = JoyReactor.SyncExecutor

module Update =
    open Elmish
    let inline map f fcmd (model, cmd) = f model, cmd |> Cmd.map fcmd

module MessagesScreen =
    open Elmish
    open JoyReactor
    open JoyReactor.Types

    type Model = { messages : Message [] }
    type Msg =
        | MessagesLoaded of Message []

    let init userName =
        { messages = [||] }
        , S.get (UserMessages.selectMessageForUser userName) |> A.exec' MessagesLoaded

    let update (model : Model) = function
        | MessagesLoaded messages -> { model with messages = messages }, Cmd.none

module ThreadsScreen =
    open Elmish
    open JoyReactor
    open JoyReactor.Types

    type Model = { threads : Message []; pageLoaded : int; notAuthorized : bool }
    type Msg =
        | LocalThreadsLoaded of Message [] option
        | NextPageLoaded of Result<Message [] * string option, exn>
        | OpenThread of Message
        | OpenAuthorization

    let private getMessages (db : LocalDb) =
        match db.userName with
        | Some _ -> Some <| UserMessages.selectThreads db.messages
        | None -> None

    let init =
        { threads = [||]; pageLoaded = 0; notAuthorized = false }
        , S.get getMessages |> A.exec' LocalThreadsLoaded

    let private syncThreads page =
        S.get (fun db -> (UserMessages.selectThreads db.messages, db.nextMessagesPage))
        |> S.withSync (UrlBuilder.messages page)
        |> A.exec NextPageLoaded

    let private tryLoadNextPage next model =
        if model.pageLoaded > 2 then Cmd.none
        else syncThreads next

    let update model = function
        | LocalThreadsLoaded (Some threads) ->
            { model with threads = threads }
            , syncThreads None
        | LocalThreadsLoaded None ->
            { model with notAuthorized = true }
            , syncThreads None
        | NextPageLoaded (Ok (threads, None)) ->
            { model with threads = threads }, Cmd.none
        | NextPageLoaded (Ok (threads, (Some _ as next))) ->
            { model with threads = threads; pageLoaded = model.pageLoaded + 1 }
            , tryLoadNextPage next model
        | NextPageLoaded (Error e) -> raise e
        | OpenThread _ -> model, Cmd.none
        | OpenAuthorization -> model, Cmd.none

module LoginScreen =
    module Domain =
        open System
        open JoyReactor
        open JoyReactor.Types

        let private mkLoginForm username password =
            [ "signin[username]", username
              "signin[password]", password ]
            |> List.fold (fun a (k, v) -> sprintf "%s&%s=%s" a k (Uri.EscapeDataString v)) ""
            |> fun form ->
                { url = sprintf "%s/login" UrlBuilder.baseUrl
                  form = form }

        let login username password =
            mkLoginForm username password
            |> A.postForm

    open Elmish

    type Model =
        { username : string
          password : string
          isEnabled : bool
          isBusy : bool
          error : string option }

    type Msg =
        | LoginMsg
        | LoginEnd of Result<unit, exn>
        | UsernameMsg of string
        | PasswordMsg of string
        | ClosePage

    let init =
        { username = ""; password = ""; isEnabled = false; isBusy = false; error = None }, Cmd.none

    let private computeIsEnable model =
        { model with isEnabled = model.username <> "" && model.password <> "" }

    let update model msg : Model * Cmd<Msg> =
        match msg with
        | LoginMsg ->
            { model with isBusy = true; error = None }
            , Domain.login model.username model.password |> Cmd.map LoginEnd
        | LoginEnd(Ok _) -> { model with isBusy = false }, Cmd.ofMsg ClosePage
        | LoginEnd(Error e) -> { model with isBusy = false; error = Some <| string e }, Cmd.none
        | UsernameMsg x ->
            { model with username = x } |> computeIsEnable
            , Cmd.none
        | PasswordMsg x ->
            { model with password = x } |> computeIsEnable
            , Cmd.none
        | _ -> model, Cmd.none

module ProfileScreen =
    open JoyReactor
    open JoyReactor.Types
    open Elmish

    type Model =
        | ProfileLoading
        | ProfileModel of Profile
        | LoginModel of LoginScreen.Model
    type Msg =
        | ProfileLoaded of Result<Profile option, exn>
        | LoginMsg of LoginScreen.Msg
        | Logout
        | LogoutEnd of Result<unit, exn>

    let private mkUserUrl (db : LocalDb) = db.userName |> Option.map UrlBuilder.user

    let init =
        ProfileLoading
        , S.get (fun db -> db.profile) |> S.withSync' mkUserUrl |> A.exec ProfileLoaded

    let update model msg =
        match msg with
        | ProfileLoaded (Ok (Some profile)) -> ProfileModel profile, Cmd.none
        | ProfileLoaded (Ok None) ->
            LoginScreen.init |> Update.map LoginModel LoginMsg
        | ProfileLoaded (Error e) -> failwithf "ProfileLoaded: %O" e
        | Logout ->
            model
            , S.sync UrlBuilder.logout |> S.withPost (always LocalDb.empty) |> A.exec LogoutEnd
        | LogoutEnd _ -> init
        | LoginMsg (LoginScreen.LoginEnd (Ok _)) -> init
        | LoginMsg childMsg ->
            match model with
            | LoginModel childModel -> LoginScreen.update childModel childMsg |> Update.map LoginModel LoginMsg
            | _ -> model, Cmd.none

module FeedScreen =
    open Elmish
    open JoyReactor
    open JoyReactor.Types

    type PostState = Actual of Post | LoadNextDivider | Old of Post

    type Model = { source: Source; items: PostState []; hasNew: bool; loading: bool; scroll: int option }

    type Msg =
        | EndScrollChange of int option
        | PostsLoadedFromCache of Result<PostsWithLevels, exn>
        | FirstPagePreloaded of Result<PostsWithLevels, exn>
        | ApplyPreloaded
        | ApplyPreloadedCompleted of PostsWithLevels
        | LoadNextPage
        | LoadNextPageCompleted of Result<PostsWithLevels, exn>
        | Refresh
        | RefreshCompleted of Result<PostsWithLevels, exn>
        | OpenPost of int

    let init source =
        { source = source; items = [||]; hasNew = false; loading = false; scroll = None }
        , S.get (fun db -> FeedMerger.getPostsWithLevels source db.feeds) |> A.exec PostsLoadedFromCache

    let private updateFeeds source (db : LocalDb) =
         let (feeds, sharedFeeds, _) = FeedMerger.mergeFirstPage source db.sharedFeeds db.feeds
         { db with feeds = feeds; sharedFeeds = sharedFeeds }
    let private getPosts source (db : LocalDb) =
         let (_, _, posts) = FeedMerger.mergeFirstPage source db.sharedFeeds db.feeds
         posts

    let update model msg =
        let toItems (ps: PostsWithLevels) loading: PostState [] =
            if Seq.isEmpty ps.preloaded && not ^ Seq.isEmpty ps.actual && not loading
                then Array.concat [ ps.actual |> Array.map Actual; [| LoadNextDivider |]; ps.old |> Array.map Old ]
                else Array.concat [ ps.actual |> Array.map Actual; ps.old |> Array.map Old ]

        match msg with
        | EndScrollChange offset -> { model with scroll = offset }, Cmd.none
        | PostsLoadedFromCache (Ok xs) ->
            { model with items = toItems xs true; loading = true }
            , S.get (getPosts model.source)
              |> S.withSync (UrlBuilder.posts model.source "FIXME" None)
              |> S.withPost (updateFeeds model.source)
              |> A.exec FirstPagePreloaded
        | PostsLoadedFromCache (Error e) -> failwithf "Error: PostsLoadedFromCache: %O" e
        | FirstPagePreloaded (Ok posts) ->
            if Array.isEmpty posts.actual && Array.isEmpty posts.old
                then
                    { model with hasNew = true; loading = false }
                    , Cmd.ofMsg ApplyPreloaded
                else
                    { model with hasNew = true; loading = false }
                    , Cmd.none
        | FirstPagePreloaded (Error e) -> failwithf "Error: FirstPagePreloaded: %O" e
        | ApplyPreloaded ->
            { model with hasNew = false }
            , S.get (fun db -> FeedMerger.getPostsWithLevels model.source db.feeds)
              |> S.withPost (fun db -> { db with feeds = FeedMerger.mergePreloaded' model.source db.feeds })
              |> A.exec' ApplyPreloadedCompleted
        | ApplyPreloadedCompleted xs ->
            { model with items = toItems xs false }
            , Cmd.none
        | LoadNextPage ->
            { model with loading = true }
            , S.get (fun db -> FeedMerger.getPostsWithLevels model.source db.feeds)
              |> S.withSync' (fun db ->
                   let a = FeedMerger.getPostsWithLevels model.source db.feeds
                   UrlBuilder.posts model.source "FIXME" a.nextPage |> Some)
              |> S.withPost (fun db ->
                   let (feeds, sharedFeeds) = FeedMerger.mergeSecondPage model.source db.feeds db.sharedFeeds
                   { db with feeds = feeds; sharedFeeds = sharedFeeds })
              |> A.exec LoadNextPageCompleted
        | LoadNextPageCompleted (Ok xs) ->
            { model with items = toItems xs false; loading = false }
            , Cmd.none
        | LoadNextPageCompleted (Error e) -> failwithf "Error: LoadNextPageCompleted: %O" e
        | Refresh ->
            { model with loading = true }
            , S.get (fun db -> FeedMerger.getPostsWithLevels model.source db.feeds)
              |> S.withSync (UrlBuilder.posts model.source "FIXME" None)
              |> S.withPost (fun db ->
                    let (feeds, sharedFeeds) = FeedMerger.replacePosts model.source db.sharedFeeds db.feeds
                    { db with feeds = feeds; sharedFeeds = sharedFeeds})
              |> A.exec RefreshCompleted
        | RefreshCompleted (Ok xs) ->
            { model with items = toItems xs false; loading = false }
            , Cmd.none
        | RefreshCompleted (Error e) -> failwithf "Error: RefreshCompleted: %O" e
        | OpenPost _ -> model, Cmd.none

module TagsScreen =
    open Elmish
    open JoyReactor
    open JoyReactor.Types

    type Model =
        { topTags : Tag []; userTags : Tag []; loaded : bool }
        with static member empty = { topTags = [||]; userTags = [||]; loaded = false }

    type Msg =
        | TopTags of Tag []
        | UserTags of Tag []
        | TopTagsSynced of Result<Tag[], exn>
        | UserTagsSynced of Result<Tag[], exn>
        | OpenTag of string

    let private topTags (db : LocalDb) = db.topTags |> Map.toArray |> Array.map snd
    let private userTags (db : LocalDb) = db.userTags |> Map.toArray |> Array.map snd
    let private mkUserUrl (db : LocalDb) = db.userName |> Option.map UrlBuilder.user

    let init =
        Model.empty
        , Cmd.batch [
            S.get topTags |> A.exec' TopTags
            S.get userTags |> A.exec' UserTags
            S.get topTags |> S.withSync UrlBuilder.home |> A.exec TopTagsSynced
            S.get userTags |> S.withSync' mkUserUrl |> A.exec UserTagsSynced ]

    let private addFavorite tags =
        Array.concat [
            [| { name = "Избранное"; image = sprintf "http://img1.%s/pics/avatar/tag/1279" UrlBuilder.domain } |]
            tags ]

    let private tagToSource tag =
        match tag.name with
        | "Избранное" -> FavoriteSource
        | _ -> TagSource tag.name

    let update (model : Model) = function
        | TopTags tags -> { model with topTags = tags }, Cmd.none
        | UserTags tags -> { model with userTags = tags }, Cmd.none
        | TopTagsSynced (Ok tags) -> { model with topTags = tags }, Cmd.none
        | UserTagsSynced (Ok tags) -> { model with userTags = tags }, Cmd.none
        | TopTagsSynced (Error _) -> model, Cmd.none
        | UserTagsSynced (Error _) -> model, Cmd.none
        | OpenTag _ -> model, Cmd.none

module TabsScreen =
    open Elmish
    open JoyReactor.Types

    type Model =
        | FeedModel of FeedScreen.Model
        | TagsModel of TagsScreen.Model
        | ThreadsModel of ThreadsScreen.Model
        | ProfileModel of ProfileScreen.Model
    type Msg =
        | FeedMsg of FeedScreen.Msg
        | TagsMsg of TagsScreen.Msg
        | ThreadsMsg of ThreadsScreen.Msg
        | ProfileMsg of ProfileScreen.Msg
        | SelectPage of int

    let init _ =
        FeedScreen.init FeedSource
        ||> fun model cmd -> FeedModel model, (cmd |> Cmd.map FeedMsg)

    let update model msg =
        match model, msg with
        | _, ThreadsMsg (ThreadsScreen.OpenAuthorization) ->
            ProfileScreen.init
            ||> fun model cmd -> ProfileModel model, (cmd |> Cmd.map ProfileMsg)
        | FeedModel model, FeedMsg msg ->
            FeedScreen.update model msg
            ||> fun model cmd -> FeedModel model, (cmd |> Cmd.map FeedMsg)
        | TagsModel model, TagsMsg msg ->
            TagsScreen.update model msg
            ||> fun model cmd -> TagsModel model, (cmd |> Cmd.map TagsMsg)
        | ThreadsModel model, ThreadsMsg msg ->
            ThreadsScreen.update model msg
            ||> fun model cmd -> ThreadsModel model, (cmd |> Cmd.map ThreadsMsg)
        | ProfileModel model, ProfileMsg msg ->
            ProfileScreen.update model msg
            ||> fun model cmd -> ProfileModel model, (cmd |> Cmd.map ProfileMsg)
        | _, SelectPage 0 ->
            FeedScreen.init FeedSource
            ||> fun model cmd -> FeedModel model, (cmd |> Cmd.map FeedMsg)
        | _, SelectPage 1 ->
            TagsScreen.init
            ||> fun model cmd -> TagsModel model, (cmd |> Cmd.map TagsMsg)
        | _, SelectPage 2 ->
            ThreadsScreen.init
            ||> fun model cmd -> ThreadsModel model, (cmd |> Cmd.map ThreadsMsg)
        | _, SelectPage 3 ->
            ProfileScreen.init
            ||> fun model cmd -> ProfileModel model, (cmd |> Cmd.map ProfileMsg)
        | _ -> model, []

module PostScreen =
    open Elmish
    open JoyReactor
    open JoyReactor.Types

    type Model = { tags: string []; image: Attachment option; isLoaded: bool; error: string option; id: int; comments: Comment [] }

    type Msg =
        | PostLoaded of Result<Post option, exn>
        | RefreshComplete of Result<Post option, exn>
        | OpenTag of string

    let getPosts id (db : LocalDb) = Map.tryFind id db.posts

    let init id =
        { tags = [||]; image = None; isLoaded = false; error = None; id = id; comments = [||] }
        , Cmd.batch [
            S.get (getPosts id) |> A.exec PostLoaded
            S.get (getPosts id) |> S.withSync (UrlBuilder.post id) |> A.exec RefreshComplete ]

    let private updateModel (model : Model) (post : Post option) =
        let comments =
            match post with
            | Some post ->
                post.comments
                |> Array.filter ^ fun comment -> comment.parentId = 0 && comment.rating >= -0.3
                |> Array.sortByDescending ^ fun comment -> comment.rating
                |> fun comments -> Array.take (min comments.Length 10) comments
                |> Array.map (fun c -> { c with image = { c.image with url = Image.origin c.image.url } })
            | None -> [||]
        let image =
            post
            |> Option.bind (fun p -> Array.tryHead p.image)
            |> Option.map (fun a -> { a with url = Image.origin a.url })
        let tags = post |> Option.map (fun p -> p.tags) |> Option.defaultValue [||]
        let isLoaded = Option.isSome post
        { model with comments = comments; image = image; tags = tags; isLoaded = isLoaded }

    let update (model : Model) = function
        | PostLoaded (Ok post) -> updateModel model post, Cmd.none
        | PostLoaded (Error e) -> failwithf "Error: PostLoaded: %O" e
        | RefreshComplete(Ok post) -> { (updateModel model post) with error = None }, Cmd.none
        | RefreshComplete(Error e) -> { model with error = Some <| string e }, Cmd.none
        | OpenTag _ -> model, Cmd.none

module ApplicationScreen =
    open Elmish
    open JoyReactor.Types

    type ChildModel =
        | PostModel of PostScreen.Model
        | PostsModel of FeedScreen.Model
        | TabsModel of TabsScreen.Model
        | MessagesModel of MessagesScreen.Model
    type Model = { history : ChildModel list; title : string }
    type Msg =
        | PostMsg of PostScreen.Msg
        | PostsMsg of FeedScreen.Msg
        | TabsMsg of TabsScreen.Msg
        | MessagesMsg of MessagesScreen.Msg
        | NavigateBack

    let init _ =
        let (m, cmd) = TabsScreen.init ()
        { history = [ TabsModel m ]; title = "JoyReactor (0.6.2)" }
        , cmd |> Cmd.map TabsMsg

    let update model msg =
        match model, msg with
        | { history = _ :: ((_ :: _) as prev) }, NavigateBack ->
            { model with history = prev }, Cmd.none
        | _, (TabsMsg (TabsScreen.FeedMsg (FeedScreen.OpenPost post))) ->
            let (m, cmd) = PostScreen.init post
            { model with history = PostModel m :: model.history }
            , cmd |> Cmd.map PostMsg
        | _, (TabsMsg (TabsScreen.TagsMsg (TagsScreen.OpenTag tag))) ->
            let (m, cmd) = FeedScreen.init ^ TagSource tag
            { model with history = PostsModel m :: model.history }
            , cmd |> Cmd.map PostsMsg
        | _, (TabsMsg (TabsScreen.ThreadsMsg (ThreadsScreen.OpenThread thread))) ->
            let (m, cmd) = MessagesScreen.init thread.userName
            { model with history = MessagesModel m :: model.history }
            , cmd |> Cmd.map MessagesMsg
        | _, (PostMsg (PostScreen.OpenTag source)) ->
            let (m, cmd) = FeedScreen.init ^ TagSource source
            { model with history = PostsModel m :: model.history }
            , cmd |> Cmd.map PostsMsg
        | _, (PostsMsg (FeedScreen.OpenPost post)) ->
            let (m, cmd) = PostScreen.init post
            { model with history = PostModel m :: model.history }
            , cmd |> Cmd.map PostMsg
        | { history = (TabsModel cmodel) :: other }, TabsMsg cmsg ->
            let (m, cmd) = TabsScreen.update cmodel cmsg
            { model with history = TabsModel m :: other }
            , cmd |> Cmd.map TabsMsg
        | { history = (PostsModel cmodel) :: other }, PostsMsg cmsg ->
            let (m, cmd) = FeedScreen.update cmodel cmsg
            { model with history = PostsModel m :: other }
            , cmd |> Cmd.map PostsMsg
        | { history = (PostModel cmodel) :: other }, PostMsg cmsg ->
            let (m, cmd) = PostScreen.update cmodel cmsg
            { model with history = PostModel m :: other }
            , cmd |> Cmd.map PostMsg
        | { history = (MessagesModel cmodel) :: other }, MessagesMsg cmsg ->
            let (m, cmd) = MessagesScreen.update cmodel cmsg
            { model with history = MessagesModel m :: other }
            , cmd |> Cmd.map MessagesMsg
        | _ -> model, Cmd.none
