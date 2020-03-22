namespace JoyReactor.Components

module ActionModule =
    open Elmish
    open JoyReactor
    open JoyReactor.Types
    type Db = CofxStorage.LocalDb

    let mutable downloadAndParseImpl : string -> ParseResponse Async = fun _ -> failwith "not implemented"
    let mutable postFormImpl : PostForm -> unit Async = fun _ -> failwith "not implemented"

    let postForm form = Cmd.OfAsync.either (fun _ -> postFormImpl form) () Ok Error

    let private db = ref Db.empty

    let run (furl: Db -> Db * string option) (callback: Db -> Db * 'a) : Result<'a, exn> Cmd =
        let invoke furl callback : _ Async =
            let downloadPostsForUrl url =
                async {
                    let! pr = downloadAndParseImpl url
                    db := DomainInterpetator.saveAllParseResults !db pr
                }
            async {
                let (ldb, opUrl) = furl !db
                db := ldb
                match opUrl with None -> () | Some url -> do! downloadPostsForUrl url
                let (ldb, result) = callback !db
                db := ldb
                return result
            }
        Cmd.OfAsync.either (fun _ -> invoke furl callback) () Ok Error

module LoginScreen =
    module Domain =
        open System
        open JoyReactor
        open JoyReactor.Types

        let private mkLoginForm username password =
            [ "signin[username]", username
              "signin[password]", password ]
            |> List.fold (fun a (k, v) -> sprintf "%s&%s=%s" a k (Uri.EscapeDataString v)) ""
            |> fun form -> { url = sprintf "%s/login" UrlBuilder.baseUrl; form = form; csrfName = "signin[_csrf_token]" }

        let login username password =
            mkLoginForm username password
            |> ActionModule.postForm

    open Elmish

    type Model =
        { username : string
          password : string
          isEnabled : bool
          isBusy : bool
          error : string option }

    type Msg =
        | LoginMsg
        | LoginResultMsg of Result<unit, exn>
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
            , Domain.login model.username model.password |> Cmd.map LoginResultMsg
        | LoginResultMsg(Ok _) -> { model with isBusy = false }, Cmd.ofMsg ClosePage
        | LoginResultMsg(Error e) -> { model with isBusy = false; error = Some <| string e }, Cmd.none
        | UsernameMsg x -> 
            { model with username = x } |> computeIsEnable
            , Cmd.none
        | PasswordMsg x -> 
            { model with password = x } |> computeIsEnable
            , Cmd.none
        | _ -> model, Cmd.none

module Update =
    open Elmish
    let inline map f fcmd (model, cmd) = f model, cmd |> Cmd.map fcmd

module ProfileScreen =
    module Domain =
        open JoyReactor
        let profile =
            ActionModule.run
                (fun db -> db, db.userName |> Option.map UrlBuilder.user)
                (fun db -> db, db.profile)

    open JoyReactor.Types
    open Elmish

    type Model =
        | ProfileLoading
        | ProfileModel of Profile
        | LoginModel of LoginScreen.Model
    type Msg = 
        | ProfileLoaded of Result<Profile option, exn>
        | LoginMsg of LoginScreen.Msg

    let init =
        ProfileLoading
        , Domain.profile |> Cmd.map ProfileLoaded

    let update model msg =
        match msg with
        | ProfileLoaded (Ok (Some profile)) -> ProfileModel profile, Cmd.none
        | ProfileLoaded (Ok None) ->
            LoginScreen.init |> Update.map LoginModel LoginMsg
        | ProfileLoaded (Error e) -> failwithf "ProfileLoaded: %O" e
        | LoginMsg childMsg ->
            match model with
            | LoginModel childModel -> LoginScreen.update childModel childMsg |> Update.map LoginModel LoginMsg
            | _ -> model, []

module FeedScreen =
    module Domain =
        open JoyReactor
        open JoyReactor.Types
        open JoyReactor.Domain''

        let init source =
            ActionModule.run 
                (fun db -> db, None)
                (fun db -> db, getPostsWithLevels' source db.feeds)
        let preloadFirstPage source =
            ActionModule.run 
                (fun db -> db, UrlBuilder.posts source "FIXME" None |> Some)
                (fun db ->
                     let (feeds, sharedFeeds, posts) = mergeFirstPage' source db.sharedFeeds db.feeds
                     { db with feeds = feeds; sharedFeeds = sharedFeeds }, posts)
        let applyPreloaded source = 
            ActionModule.run 
                (fun db -> db, None)
                (fun db -> 
                     let db = { db with feeds = mergePreloaded' source db.feeds }
                     db, getPostsWithLevels' source db.feeds)
        let loadNextPage source = 
            ActionModule.run 
                (fun db ->
                     let a = getPostsWithLevels' source db.feeds
                     db, UrlBuilder.posts source "FIXME" a.nextPage |> Some)
                (fun db ->
                     let (feeds, sharedFeeds) = mergeSecondPage' source db.feeds db.sharedFeeds
                     let db = { db with feeds = feeds; sharedFeeds = sharedFeeds }
                     db, getPostsWithLevels' source db.feeds)
        let refresh source = 
            ActionModule.run 
                (fun db -> db, UrlBuilder.posts source "FIXME" None |> Some)
                (fun db ->
                     let (feeds, sharedFeeds) = replacePosts' source db.sharedFeeds db.feeds
                     let db = { db with feeds = feeds; sharedFeeds = sharedFeeds}
                     db, getPostsWithLevels' source db.feeds)

    open Elmish
    open JoyReactor.Types

    type PostState = | Actual of Post | LoadNextDivider | Old of Post

    type Model = { source: Source; items: PostState []; hasNew: bool; loading: bool }
    type Msg =
        | PostsLoadedFromCache of Result<PostsWithLevels, exn>
        | FirstPagePreloaded of Result<PostsWithLevels, exn>
        | ApplyPreloaded
        | ApplyPreloadedCompleted of Result<PostsWithLevels, exn>
        | LoadNextPage
        | LoadNextPageCompleted of Result<PostsWithLevels, exn>
        | Refresh
        | RefreshCompleted of Result<PostsWithLevels, exn>
        | OpenPost of Post

    let init source =
        { source = source; items = [||]; hasNew = false; loading = false }
        , Domain.init source |> Cmd.map PostsLoadedFromCache

    let update model msg =
        let toItems (ps: PostsWithLevels) loading: PostState [] =
            if Seq.isEmpty ps.preloaded && not ^ Seq.isEmpty ps.actual && not loading
                then Array.concat [ ps.actual |> Array.map Actual; [| LoadNextDivider |]; ps.old |> Array.map Old ]
                else Array.concat [ ps.actual |> Array.map Actual; ps.old |> Array.map Old ]

        match msg with
        | PostsLoadedFromCache (Ok xs) ->
            { model with items = toItems xs true; loading = true }
            , Domain.preloadFirstPage model.source |> Cmd.map FirstPagePreloaded
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
            , Domain.applyPreloaded model.source |> Cmd.map ApplyPreloadedCompleted
        | ApplyPreloadedCompleted (Ok xs) -> 
            { model with items = toItems xs false }
            , Cmd.none
        | ApplyPreloadedCompleted (Error e) -> failwithf "Error: ApplyPreloadedCompleted: %O" e
        | LoadNextPage ->
            { model with loading = true }
            , Domain.loadNextPage model.source |> Cmd.map LoadNextPageCompleted
        | LoadNextPageCompleted (Ok xs) -> 
            { model with items = toItems xs false; loading = false }
            , Cmd.none
        | LoadNextPageCompleted (Error e) -> failwithf "Error: LoadNextPageCompleted: %O" e
        | Refresh ->
            { model with loading = true }
            , Domain.refresh model.source |> Cmd.map RefreshCompleted
        | RefreshCompleted (Ok xs) -> 
            { model with items = toItems xs false; loading = false }
            , Cmd.none
        | RefreshCompleted (Error e) -> failwithf "Error: RefreshCompleted: %O" e
        | OpenPost _ -> model, Cmd.none

module TagsScreen =
    module Domain =
        open JoyReactor
        open JoyReactor.CofxStorage

        let private sub (db : LocalDb) = 
            [ db.topTags |> Map.toSeq |> Seq.map snd |> Seq.toArray
              db.userTags  |> Map.toSeq |> Seq.map snd |> Seq.toArray ] 
            |> Array.concat
        let fromCache =
            ActionModule.run (fun db -> db, None) (fun db -> db, sub db)
        let userTags =
            ActionModule.run 
                (fun db -> db, db.userName |> Option.map ^ UrlBuilder.user) 
                (fun db -> db, sub db)
        let topTags = 
            ActionModule.run (fun db -> db, Some UrlBuilder.home) (fun db -> db, sub db)

    open Elmish
    open JoyReactor
    open JoyReactor.Types
    type LocalDb = CofxStorage.LocalDb

    type Model =
        { tags : Tag []; loaded : bool }
        with static member empty = { tags = [||]; loaded = false }

    type Msg =
        | Refresh
        | RefreshComplete of Result<Tag[], exn>
        | FromCacheMsg of Result<Tag[], exn>
        | OpenTag of Tag

    let private refresh model =
        { model with loaded = false }
        , Cmd.batch [
            Domain.topTags |> Cmd.map RefreshComplete
            Domain.userTags |> Cmd.map RefreshComplete ]

    let init _ = 
        let (model, action) = refresh Model.empty
        model, (Domain.fromCache |> Cmd.map FromCacheMsg) @ action

    let private addFavorite tags =
        Array.concat [
            [| { name = "Избранное"; image = sprintf "http://img1.%s/pics/avatar/tag/1279" UrlBuilder.domain } |]
            tags ]

    let private tagToSource tag =
        match tag.name with
        | "Избранное" -> FavoriteSource
        | _ -> TagSource tag.name

    let update (model : Model) = function
        | Refresh -> refresh model
        | FromCacheMsg (Ok tags) -> 
            { model with tags = addFavorite tags }
            , Cmd.none
        | FromCacheMsg (Error e) -> failwithf "FromCacheMsg %O" e
        | RefreshComplete (Ok tags) -> 
            { model with loaded = true; tags = addFavorite tags }
            , Cmd.none
        | RefreshComplete (Error e) -> failwithf "RefreshComplete %O" e
        | OpenTag _ -> model, Cmd.none

module TabsScreen =
    open Elmish
    open JoyReactor.Types

    type Model =
        | FeedModel of FeedScreen.Model
        | TagsModel of TagsScreen.Model
        | ProfileModel of ProfileScreen.Model
    type Msg = 
        | FeedMsg of FeedScreen.Msg
        | TagsMsg of TagsScreen.Msg
        | ProfileMsg of ProfileScreen.Msg
        | SelectPage of int

    let init _ =
        FeedScreen.init FeedSource
        ||> fun model cmd -> FeedModel model, (cmd |> Cmd.map FeedMsg)

    let update model msg =
        match model, msg with
        | ProfileModel model, ProfileMsg msg -> 
            ProfileScreen.update model msg
            ||> fun model cmd -> ProfileModel model, (cmd |> Cmd.map ProfileMsg)
        | FeedModel model, FeedMsg msg -> 
            FeedScreen.update model msg
            ||> fun model cmd -> FeedModel model, (cmd |> Cmd.map FeedMsg)
        | TagsModel model, TagsMsg msg -> 
            TagsScreen.update model msg
            ||> fun model cmd -> TagsModel model, (cmd |> Cmd.map TagsMsg)
        | _, SelectPage 0 ->
            FeedScreen.init FeedSource
            ||> fun model cmd -> FeedModel model, (cmd |> Cmd.map FeedMsg)
        | _, SelectPage 1 ->
            TagsScreen.init ()
            ||> fun model cmd -> TagsModel model, (cmd |> Cmd.map TagsMsg)
        | _, SelectPage 3 ->
            ProfileScreen.init
            ||> fun model cmd -> ProfileModel model, (cmd |> Cmd.map ProfileMsg)
        | _ -> model, []

module PostScreen =
    module Domain =
        open JoyReactor
        let fromCache id =
            ActionModule.run (fun db -> db, None) (fun db -> db, Map.tryFind id db.posts)
        let post id =
            ActionModule.run (fun db -> db, Some ^ UrlBuilder.post id) (fun db -> db, Map.tryFind id db.posts)

    open Elmish
    open JoyReactor
    open JoyReactor.Types

    type Model = { post : Post option; error : string option; id : int }

    type Msg =
        | PostLoaded of Result<Post option, exn>
        | RefreshComplete of Result<Post option, exn>
        | OpenInWeb
        | OpenTag of Source

    let init id = 
        { post = None; error = None; id = id }
        , Cmd.batch [
            Domain.fromCache id |> Cmd.map PostLoaded
            Domain.post id |> Cmd.map RefreshComplete ]

    let update (model : Model) = function
        | PostLoaded (Ok post) -> { model with post = post }, Cmd.none
        | PostLoaded (Error e) -> failwithf "Error: PostLoaded: %O" e
        | RefreshComplete(Ok post) -> { model with error = None; post = post }, Cmd.none
        | RefreshComplete(Error e) -> { model with error = Some <| string e }, Cmd.none
        | OpenInWeb ->
            model
            , failwith "???"
            // , sprintf "http://m.%s/post/%i" UrlBuilder.domain model.id
            //   |> Platform.openUrl |> Cmd.ofEffect0
        | _ -> model, Cmd.none

module StackNavigationComponent =
    open Elmish
    open JoyReactor.Types

    type ChildModel = 
        | PostModel of PostScreen.Model
        | PostsModel of FeedScreen.Model
        | TabsModel of TabsScreen.Model
    type Model = { history : ChildModel list }
    type Msg = 
        | PostMsg of PostScreen.Msg
        | PostsMsg of FeedScreen.Msg
        | TabsMsg of TabsScreen.Msg

    let init _ =
        let (m, cmd) = TabsScreen.init ()
        { history = [ TabsModel m ] }
        , cmd |> Cmd.map TabsMsg

    let update model msg =
        match model, msg with
        | _, (TabsMsg (TabsScreen.FeedMsg (FeedScreen.OpenPost post))) -> 
            let (m, cmd) = PostScreen.init post.id
            { history = PostModel m :: model.history }
            , cmd |> Cmd.map PostMsg
        | _, (TabsMsg (TabsScreen.TagsMsg (TagsScreen.OpenTag tag))) -> 
            let (m, cmd) = FeedScreen.init ^ TagSource tag.name
            { history = PostsModel m :: model.history }
            , cmd |> Cmd.map PostsMsg
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
        | _ -> model, Cmd.none
