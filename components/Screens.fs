namespace JoyReactor.Components

module ActionModule =
    open Elmish
    open JoyReactor
    open JoyReactor.Types
    type LocalDb = CofxStorage.LocalDb

    let mutable downloadAndParse : string -> ParseResponse Async = fun _ -> failwith "not implemented"

    let private db = ref LocalDb.empty

    let private invoke furl callback : _ Async =
        let downloadPostsForUrl url =
            async {
                let! pr = downloadAndParse url
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

    let run (furl: LocalDb -> LocalDb * string option) (callback: LocalDb -> LocalDb * 'a) : Result<'a, exn> Cmd =
        Cmd.OfAsync.either (fun _ -> invoke furl callback) () Ok Error

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
                     let (feeds, sharedFeeds) = mergeFirstPage' source db.sharedFeeds db.feeds
                     { db with feeds = feeds; sharedFeeds = sharedFeeds }, ())
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
        | FirstPagePreloaded of Result<unit, exn>
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
        | FirstPagePreloaded _ -> 
            { model with hasNew = true; loading = false }
            , Cmd.none
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
    type Msg = 
        | FeedMsg of FeedScreen.Msg
        | TagsMsg of TagsScreen.Msg
        | SelectPage of int
    let init _ =
        FeedScreen.init FeedSource
        ||> fun model cmd -> FeedModel model, (cmd |> Cmd.map FeedMsg)
    let update model msg =
        match model, msg with
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
        | _ -> model, []

module StackNavigationComponent =
    open Elmish
    open JoyReactor.Types

    type ChildModel = 
        | PostsModel of FeedScreen.Model
        | TabsModel of TabsScreen.Model
    type Model = { history : ChildModel list }
    type Msg = 
        | PostsMsg of FeedScreen.Msg
        | TabsMsg of TabsScreen.Msg

    let init _ =
        let (m, cmd) = TabsScreen.init ()
        { history = [ TabsModel m ] }
        , cmd |> Cmd.map TabsMsg

    let update model msg =
        match model, msg with
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
        | _ -> model, Cmd.none
