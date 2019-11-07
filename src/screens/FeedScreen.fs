module JR.Screens.FeedScreen

module Domain =
    open JoyReactor
    open JoyReactor.Types
    open JoyReactor.SyncDomain

    let private getPostsWithLevels source (db : LocalDb) =
        Map.tryFind source db.feeds
        |> Option.defaultValue PostsWithLevels.empty

    let init source =
        { url = fun db -> db, None
          callback = fun db -> db, getPostsWithLevels source db }

    let preloadFirstPage source =
        let mergeFirstPage db =
            let old = getPostsWithLevels source db
            let preloaded = db.sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = None }
            let a = { old with preloaded = preloaded.posts; nextPage = preloaded.nextPage }
            { db with feeds = Map.add source a db.feeds; sharedFeeds = None }

        { url = fun db -> db, UrlBuilder.posts source "FIXME" None |> Some
          callback = fun db -> mergeFirstPage db, () }

    let applyPreloaded source = 
        let mergePreloaded db =
            let old = getPostsWithLevels source db
            let ids = old.preloaded |> (Seq.map ^ fun x -> x.id) |> Set.ofSeq
            let a = { old with
                        actual = old.preloaded
                        old = old.actual |> (Seq.filter ^ fun x -> not ^ Seq.contains x.id ids) |> Seq.toArray }
            { db with feeds = Map.add source a db.feeds }

        { url = fun db -> db, None
          callback = fun db -> 
            let db = mergePreloaded db
            db, getPostsWithLevels source db }

    let loadNextPage source = 
        let mergeSecondPage db =
            let merge old posts =
                Array.concat [ old.actual; posts ]
                |> Array.distinctBy ^ fun x -> x.id
            let mergeOld old posts =
                let keys = posts |> (Array.map ^ fun x -> x.id) |> Set.ofArray
                old.old |> Array.filter ^ fun x -> Set.contains x.id keys

            let old = getPostsWithLevels source db
            let response = db.sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = None }
            let a = { old with 
                        actual = merge old response.posts
                        old = mergeOld old response.posts
                        nextPage = response.nextPage }
            { db with 
                feeds = Map.add source a db.feeds
                sharedFeeds = None }
    
        { url = fun db ->
            let a = getPostsWithLevels source db
            db, UrlBuilder.posts source "FIXME" a.nextPage |> Some
          callback = fun db -> 
            let db = mergeSecondPage db
            db, getPostsWithLevels source db }

    let refresh source = 
        let replacePosts (db : LocalDb) =
            let response = db.sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = None }
            let a = { actual = response.posts
                      old = [||]
                      preloaded = [||]
                      nextPage = response.nextPage }
            { db with 
                feeds = Map.add source a db.feeds
                sharedFeeds = None }

        { url = fun db -> db, UrlBuilder.posts source "FIXME" None |> Some
          callback = fun db ->
            let db = replacePosts db
            db, getPostsWithLevels source db }

open Elmish
open JoyReactor.Types
module R = JoyReactor.Services.EffRuntime

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

let init source =
    { source = source; items = [||]; hasNew = false; loading = false },
    Domain.init source |> R.run |> Cmd.map PostsLoadedFromCache

let update model msg =
    let toItems (ps: PostsWithLevels) loading: PostState [] =
        if Seq.isEmpty ps.preloaded && not ^ Seq.isEmpty ps.actual && not loading
            then Array.concat [ ps.actual |> Array.map Actual; [| LoadNextDivider |]; ps.old |> Array.map Old ]
            else Array.concat [ ps.actual |> Array.map Actual; ps.old |> Array.map Old ]

    match msg with
    | PostsLoadedFromCache(Ok xs) ->
        { model with items = toItems xs true; loading = true },
        Domain.preloadFirstPage model.source |> R.run |> Cmd.map FirstPagePreloaded
    | FirstPagePreloaded(Ok _) -> { model with hasNew = true; loading = false }, Cmd.none
    | ApplyPreloaded -> 
        { model with hasNew = false }, 
        Domain.applyPreloaded model.source |> R.run |> Cmd.map ApplyPreloadedCompleted
    | ApplyPreloadedCompleted (Ok xs) -> { model with items = toItems xs false }, Cmd.none
    | LoadNextPage ->
        { model with loading = true },
        Domain.loadNextPage model.source |> R.run |> Cmd.map LoadNextPageCompleted
    | LoadNextPageCompleted (Ok xs) -> { model with items = toItems xs false; loading = false }, Cmd.none
    | Refresh ->
        { model with loading = true },
        Domain.refresh model.source |> R.run |> Cmd.map RefreshCompleted
    | RefreshCompleted (Ok xs) -> { model with items = toItems xs false; loading = false }, Cmd.none
    | x -> failwithf "Not implemented = %O" x
