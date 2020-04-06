namespace JoyReactor.Components

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
        , Services.getMessages userName |> Cmd.map MessagesLoaded

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

    let init =
        { threads = [||]; pageLoaded = 0; notAuthorized = false }
        , Services.getThreads |> Cmd.map LocalThreadsLoaded

    let private tryLoadNextPage next model =
        if model.pageLoaded > 2 then Cmd.none
        else Services.syncThreads next |> Cmd.map NextPageLoaded

    let update model = function
        | LocalThreadsLoaded (Some threads) -> 
            { model with threads = threads }
            , Services.syncThreads None |> Cmd.map NextPageLoaded
        | LocalThreadsLoaded None -> 
            { model with notAuthorized = true }
            , Services.syncThreads None |> Cmd.map NextPageLoaded
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

    let init =
        ProfileLoading
        , Services.profile |> Cmd.map ProfileLoaded

    let update model msg =
        match msg with
        | ProfileLoaded (Ok (Some profile)) -> ProfileModel profile, Cmd.none
        | ProfileLoaded (Ok None) ->
            LoginScreen.init |> Update.map LoginModel LoginMsg
        | ProfileLoaded (Error e) -> failwithf "ProfileLoaded: %O" e
        | Logout -> model, Services.logout |> Cmd.map LogoutEnd
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
        , FeedServices.init source |> Cmd.map PostsLoadedFromCache

    let update model msg =
        let toItems (ps: PostsWithLevels) loading: PostState [] =
            if Seq.isEmpty ps.preloaded && not ^ Seq.isEmpty ps.actual && not loading
                then Array.concat [ ps.actual |> Array.map Actual; [| LoadNextDivider |]; ps.old |> Array.map Old ]
                else Array.concat [ ps.actual |> Array.map Actual; ps.old |> Array.map Old ]

        match msg with
        | PostsLoadedFromCache (Ok xs) ->
            { model with items = toItems xs true; loading = true }
            , FeedServices.preloadFirstPage model.source |> Cmd.map FirstPagePreloaded
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
            , FeedServices.applyPreloaded model.source |> Cmd.map ApplyPreloadedCompleted
        | ApplyPreloadedCompleted (Ok xs) -> 
            { model with items = toItems xs false }
            , Cmd.none
        | ApplyPreloadedCompleted (Error e) -> failwithf "Error: ApplyPreloadedCompleted: %O" e
        | LoadNextPage ->
            { model with loading = true }
            , FeedServices.loadNextPage model.source |> Cmd.map LoadNextPageCompleted
        | LoadNextPageCompleted (Ok xs) -> 
            { model with items = toItems xs false; loading = false }
            , Cmd.none
        | LoadNextPageCompleted (Error e) -> failwithf "Error: LoadNextPageCompleted: %O" e
        | Refresh ->
            { model with loading = true }
            , FeedServices.refresh model.source |> Cmd.map RefreshCompleted
        | RefreshCompleted (Ok xs) -> 
            { model with items = toItems xs false; loading = false }
            , Cmd.none
        | RefreshCompleted (Error e) -> failwithf "Error: RefreshCompleted: %O" e
        | OpenPost _ -> model, Cmd.none

module TagsScreen =
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
            Services.topTags |> Cmd.map RefreshComplete
            Services.userTags |> Cmd.map RefreshComplete ]

    let init _ = 
        let (model, action) = refresh Model.empty
        model, (Services.tagFromCache |> Cmd.map FromCacheMsg) @ action

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
            TagsScreen.init ()
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

    type Model = { post : Post option; error : string option; id : int; comments : Comment [] }

    type Msg =
        | PostLoaded of Result<Post option, exn>
        | RefreshComplete of Result<Post option, exn>
        | OpenTag of string

    let init id = 
        { post = None; error = None; id = id; comments = [||] }
        , Cmd.batch [
            Services.postFromCache id |> Cmd.map PostLoaded
            Services.post id |> Cmd.map RefreshComplete ]

    let private updateModel (model : Model) (post : Post option) =
        let comments =
            match post with
            | Some post ->
                post.comments
                |> Array.sortByDescending ^ fun comment -> comment.rating
                |> Array.take (min post.comments.Length 10)
            | None -> [||]
        { model with post = post; comments = comments }

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
    type Model = { history : ChildModel list }
    type Msg = 
        | PostMsg of PostScreen.Msg
        | PostsMsg of FeedScreen.Msg
        | TabsMsg of TabsScreen.Msg
        | MessagesMsg of MessagesScreen.Msg

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
        | _, (TabsMsg (TabsScreen.ThreadsMsg (ThreadsScreen.OpenThread thread))) -> 
            let (m, cmd) = MessagesScreen.init thread.userName
            { history = MessagesModel m :: model.history }
            , cmd |> Cmd.map MessagesMsg
        | _, (PostMsg (PostScreen.OpenTag source)) -> 
            let (m, cmd) = FeedScreen.init ^ TagSource source
            { history = PostsModel m :: model.history }
            , cmd |> Cmd.map PostsMsg
        | _, (PostsMsg (FeedScreen.OpenPost post)) -> 
            let (m, cmd) = PostScreen.init post.id
            { history = PostModel m :: model.history }
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
