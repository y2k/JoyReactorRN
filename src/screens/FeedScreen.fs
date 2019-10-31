module JR.Screens.FeedScreen

module Domain =
    open JoyReactor
    open JoyReactor.Types
    open JoyReactor.SyncDomain

    let getPostsWithLevels source (db: LocalDb) =
        Map.tryFind source db.feeds
        |> Option.defaultValue PostsWithLevels.empty

    let init source =
        { url = fun db -> db, None
          callback = fun db -> db, getPostsWithLevels source db }

    let preloadFirstPage source =
        let mergeFirstPage db source =
            let old = getPostsWithLevels source db
            let preloaded = Map.tryFind source db.sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = None }
            let a = { old with preloaded = preloaded.posts; nextPage = preloaded.nextPage }
            { db with feeds = Map.add source a db.feeds; sharedFeeds = Map.remove source db.sharedFeeds }

        { url = fun db -> db, UrlBuilder.posts source "FIXME" None |> Some
          callback = fun db -> mergeFirstPage db source, () }

    let applyPreloaded source = 
        let mergePreloaded db source = failwith "TODO"

        { url = fun db -> db, None
          callback = fun db -> 
            let db = mergePreloaded db source
            db, getPostsWithLevels source db }

    let loadNextPage source = 
        let mergeSecondPage db source = failwith "TODO"
    
        { url = fun db ->
            let a = getPostsWithLevels source db
            db, UrlBuilder.posts source "FIXME" a.nextPage |> Some
          callback = fun db -> 
            let db = mergeSecondPage db source
            db, getPostsWithLevels source db }

    let refresh source = 
        let replacePosts db source = failwith "TODO"

        { url = fun db -> db, UrlBuilder.posts source "FIXME" None |> Some
          callback = fun db ->
            let db = replacePosts db source
            db, getPostsWithLevels source db }

open Elmish
open JoyReactor.Types
module R = JoyReactor.Services.EffRuntime

type PostState = | Actual of Post | Divider | Old of Post

type Model = { source: Source; items: PostState []; hasNew: bool }
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
    { source = source; items = [||]; hasNew = false },
    Domain.init source |> R.run |> Cmd.map PostsLoadedFromCache

let update model msg =
    let toItems (ps: PostsWithLevels): PostState [] =
        if Seq.isEmpty ps.preloaded
            then Array.concat [ ps.actual |> Array.map Actual; [| Divider |]; ps.old |> Array.map Old ]
            else Array.concat [ ps.actual |> Array.map Actual; ps.old |> Array.map Old ]

    match msg with
    | PostsLoadedFromCache(Ok xs) ->
        { model with items = toItems xs },
        Domain.preloadFirstPage model.source |> R.run |> Cmd.map FirstPagePreloaded
    | FirstPagePreloaded(Ok _) -> { model with hasNew = true }, Cmd.none
    | ApplyPreloaded -> 
        { model with hasNew = false }, 
        Domain.applyPreloaded model.source |> R.run |> Cmd.map ApplyPreloadedCompleted
    | ApplyPreloadedCompleted (Ok xs) -> { model with items = toItems xs }, Cmd.none
    | LoadNextPage -> model, Domain.loadNextPage model.source |> R.run |> Cmd.map LoadNextPageCompleted
    | Refresh -> model, Domain.refresh model.source |> R.run |> Cmd.map RefreshCompleted
    | RefreshCompleted (Ok xs) -> { model with items = toItems xs }, Cmd.none
    | _ -> model, Cmd.none
