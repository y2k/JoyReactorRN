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

    let mergeFirstPage db source = failwith "TODO"

    let loadFirstPage source =
        { url = fun db -> db, UrlBuilder.posts source "FIXME" None |> Some
          callback = fun db -> mergeFirstPage db source, () }

open Elmish
open JoyReactor.Types
module R = JoyReactor.Services.EffRuntime

type PostState = | Actual of Post | Divider | Old of Post

type Model = { source: Source; items: PostState []; hasNew: bool }
type Msg =
    | PostsLoadedFromCache of Result<PostsWithLevels, exn>
    | FirstPageSynced of Result<unit, exn>

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
        Domain.loadFirstPage model.source |> R.run |> Cmd.map FirstPageSynced
    | FirstPageSynced(Ok _) ->
        { model with hasNew = true },
        Cmd.none
    | _ -> model, Cmd.none
