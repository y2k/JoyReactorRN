module JoyReactor.Components.FeedScreen

module Action =
    open JoyReactor.CofxStorage

    type 'a Action =
        | NoneAction
        | Eff of url: (LocalDb -> LocalDb * string option) * callback: (LocalDb -> LocalDb * 'a)
    let none () : 'a Action = NoneAction
    let map (f : Result<'a, exn> -> 'b) (a : 'a Action) : 'b Action =
        match a with
        | NoneAction -> NoneAction
        | Eff (url, callback) -> Eff (url, (fun db -> let (db, a) = callback db in db, f (Ok a)))

module Domain =
    open Action
    open JoyReactor
    open JoyReactor.Types
    open JoyReactor.Domain''

    let init source =
        Eff ( (fun db -> db, None)
            , (fun db -> db, getPostsWithLevels' source db.feeds) )
    let preloadFirstPage source =
        Eff ( (fun db -> db, UrlBuilder.posts source "FIXME" None |> Some)
            , (fun db ->
                 let (feeds, sharedFeeds) = mergeFirstPage' source db.sharedFeeds db.feeds
                 { db with feeds = feeds; sharedFeeds = sharedFeeds }, ()) )
    let applyPreloaded source = 
        Eff ( (fun db -> db, None)
            , (fun db -> 
                 let db = { db with feeds = mergePreloaded' source db.feeds }
                 db, getPostsWithLevels' source db.feeds) )
    let loadNextPage source = 
        Eff ( (fun db ->
                 let a = getPostsWithLevels' source db.feeds
                 db, UrlBuilder.posts source "FIXME" a.nextPage |> Some)
            , (fun db ->
                 let (feeds, sharedFeeds) = mergeSecondPage' source db.feeds db.sharedFeeds
                 let db = { db with feeds = feeds; sharedFeeds = sharedFeeds }
                 db, getPostsWithLevels' source db.feeds) )
    let refresh source = 
        Eff ( (fun db -> db, UrlBuilder.posts source "FIXME" None |> Some)
            , (fun db ->
                 let (feeds, sharedFeeds) = replacePosts' source db.sharedFeeds db.feeds
                 let db = { db with feeds = feeds; sharedFeeds = sharedFeeds}
                 db, getPostsWithLevels' source db.feeds) )

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
    , Domain.init source |> Action.map PostsLoadedFromCache

let update model msg =
    let toItems (ps: PostsWithLevels) loading: PostState [] =
        if Seq.isEmpty ps.preloaded && not ^ Seq.isEmpty ps.actual && not loading
            then Array.concat [ ps.actual |> Array.map Actual; [| LoadNextDivider |]; ps.old |> Array.map Old ]
            else Array.concat [ ps.actual |> Array.map Actual; ps.old |> Array.map Old ]

    match msg with
    | PostsLoadedFromCache (Ok xs) ->
        { model with items = toItems xs true; loading = true }
        , Domain.preloadFirstPage model.source |> Action.map FirstPagePreloaded
    | PostsLoadedFromCache (Error e) -> failwithf "Error: PostsLoadedFromCache: %O" e
    | FirstPagePreloaded _ -> 
        { model with hasNew = true; loading = false }
        , Action.none()
    | ApplyPreloaded -> 
        { model with hasNew = false }
        , Domain.applyPreloaded model.source |> Action.map ApplyPreloadedCompleted
    | ApplyPreloadedCompleted (Ok xs) -> 
        { model with items = toItems xs false }
        , Action.none()
    | ApplyPreloadedCompleted (Error e) -> failwithf "Error: ApplyPreloadedCompleted: %O" e
    | LoadNextPage ->
        { model with loading = true }
        , Domain.loadNextPage model.source |> Action.map LoadNextPageCompleted
    | LoadNextPageCompleted (Ok xs) -> 
        { model with items = toItems xs false; loading = false }
        , Action.none()
    | LoadNextPageCompleted (Error e) -> failwithf "Error: LoadNextPageCompleted: %O" e
    | Refresh ->
        { model with loading = true }
        , Domain.refresh model.source |> Action.map RefreshCompleted
    | RefreshCompleted (Ok xs) -> 
        { model with items = toItems xs false; loading = false }
        , Action.none()
    | RefreshCompleted (Error e) -> failwithf "Error: RefreshCompleted: %O" e
    | OpenPost _ -> model, Action.none()
