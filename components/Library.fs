module JoyReactor.Components.FeedScreen

module Actions =
    open Elmish
    open JoyReactor.Types

    type 'a Action =
        | InitAction of Source * (Result<PostsWithLevels, exn> -> 'a)
        | PreloadFirstPageAction of Source * (Result<unit, exn> -> 'a)
        | ApplyPreloadedAction of Source * (Result<PostsWithLevels, exn> -> 'a)
        | LoadNextPageAction of Source * (Result<PostsWithLevels, exn> -> 'a)
        | RefreshAction of Source * (Result<PostsWithLevels, exn> -> 'a)

    module Cmd =
        let fromAction (_ : 'a Action) : 'a Cmd = failwith "???"

open Elmish
open JoyReactor.Types
open Actions

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
    | IgnoreSubMessage

let init source =
    { source = source; items = [||]; hasNew = false; loading = false }
    , InitAction (source, PostsLoadedFromCache) |> Cmd.fromAction
    // Domain.init source |> R.run |> Cmd.map PostsLoadedFromCache

let update model msg =
    let toItems (ps: PostsWithLevels) loading: PostState [] =
        if Seq.isEmpty ps.preloaded && not ^ Seq.isEmpty ps.actual && not loading
            then Array.concat [ ps.actual |> Array.map Actual; [| LoadNextDivider |]; ps.old |> Array.map Old ]
            else Array.concat [ ps.actual |> Array.map Actual; ps.old |> Array.map Old ]

    match msg with
    | PostsLoadedFromCache(Ok xs) ->
        { model with items = toItems xs true; loading = true }
        , PreloadFirstPageAction (model.source, FirstPagePreloaded) |> Cmd.fromAction
        // , Domain.preloadFirstPage model.source |> R.run |> Cmd.map FirstPagePreloaded
    | FirstPagePreloaded(Ok _) -> 
        { model with hasNew = true; loading = false }, Cmd.none
    | ApplyPreloaded -> 
        { model with hasNew = false }
        , ApplyPreloadedAction (model.source, ApplyPreloadedCompleted) |> Cmd.fromAction
        // , Domain.applyPreloaded model.source |> R.run |> Cmd.map ApplyPreloadedCompleted
    | ApplyPreloadedCompleted (Ok xs) -> 
        { model with items = toItems xs false }, Cmd.none
    | LoadNextPage ->
        { model with loading = true }
        , LoadNextPageAction (model.source, LoadNextPageCompleted) |> Cmd.fromAction
        // , Domain.loadNextPage model.source |> R.run |> Cmd.map LoadNextPageCompleted
    | LoadNextPageCompleted (Ok xs) -> 
        { model with items = toItems xs false; loading = false }, Cmd.none
    | Refresh ->
        { model with loading = true }
        , RefreshAction (model.source, RefreshCompleted) |> Cmd.fromAction
        // , Domain.refresh model.source |> R.run |> Cmd.map RefreshCompleted
    | RefreshCompleted (Ok xs) -> 
        { model with items = toItems xs false; loading = false }, Cmd.none
    | IgnoreSubMessage -> model, Cmd.none
    | _ -> failwithf "Not implemented = %O" msg
