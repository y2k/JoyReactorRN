namespace JoyReactor
open CofxStorage

module UrlBuilder =
    open Fable.Core.JS
    open Types

    let baseUrl = sprintf "http://%s" UrlBuilder.domain
    let home = sprintf "%s/" baseUrl
    let donate = sprintf "%s/donate" baseUrl
    let ads = sprintf "%s/ads" baseUrl

    let messages page =
        page
        |> Option.defaultValue "/private/list"
        |> (+) baseUrl

    let user userName = encodeURIComponent userName |> sprintf "%s/user/%s" baseUrl
    let post id = sprintf "%s/post/%i" baseUrl id

    let posts source userName (page : int option) =
        match source with
        | FeedSource ->
            page
            |> Option.map string
            |> Option.defaultValue ""
            |> (+) (baseUrl + "/")
        | TagSource name ->
            page
            |> Option.map (sprintf "/%i")
            |> Option.defaultValue ""
            |> (+) (sprintf "%s/tag/%s" baseUrl name)
        | FavoriteSource ->
            page
            |> Option.map (sprintf "/%i")
            |> Option.defaultValue ""
            |> (+) (sprintf "%s/user/%s/favorite" baseUrl userName)

module Domain =
    open System.Text.RegularExpressions
    open Types

    let extractName html =
        let m = Regex.Match(html, "<a href=\"/user/([^\"]+)\"\\s+id=\"settings\"")
        if m.Success then Some m.Groups.[1].Value else None
        |> Option.map UrlBuilder.user

    let sourceToString =
        function
        | FeedSource -> "posts"
        | TagSource name -> "posts-" + name
        | FavoriteSource -> "my-favorites"

    let getCsrfToken html =
        let m = Regex.Match(html, "name=\"signin\\[_csrf_token\\]\" value=\"([^\"]+)")
        if m.Success then Some <| m.Groups.[1].Value
        else None

    let selectThreads messages =
        messages
        |> Seq.toArray
        |> Array.sortByDescending (fun x -> x.date)
        |> Array.distinctBy (fun x -> x.userName)

    let selectMessageForUser userName messages =
        messages
        |> Seq.filter (fun x -> x.userName = userName)
        |> Seq.toArray
        |> Array.sortByDescending (fun x -> x.date)

    let filterNewMessages (messages : Message []) offset = messages |> Array.filter (fun x -> x.date > offset)
    let checkMessagesIsOld (messages : Message []) offset = messages |> Array.exists (fun x -> x.date <= offset)

    let getLastOffsetOrDefault = Array.fold (fun a x -> max a x.date) 0.

    let private isStop messages lastOffset nextPage newMessages =
        let flagIsStop = checkMessagesIsOld messages lastOffset
        flagIsStop || Option.isNone nextPage || Array.length newMessages >= 200

    let mergeMessages parentMessages messages nextPage =
        let lastOffset = getLastOffsetOrDefault parentMessages
        let newMessages = Array.append parentMessages (filterNewMessages messages lastOffset)
        let stop = isStop messages lastOffset nextPage newMessages
        newMessages, stop

module SyncDomain =
    type LocalDb = JoyReactor.CofxStorage.LocalDb

    type Eff<'a> =    
        { url: LocalDb -> LocalDb * string option
          callback: LocalDb -> LocalDb * 'a }

    let messages page =
        { url = fun db -> 
              { db with sharedMessages = Set.empty },
              UrlBuilder.messages page |> Some
          callback = fun db -> 
              { db with messages = Set.union db.messages db.sharedMessages },
              db.nextMessagesPage }

    let logout =
        { url = fun db -> db, sprintf "%s/logout" UrlBuilder.baseUrl |> Some
          callback = fun db -> db, () }

    let profile =
        { url = fun db -> db, db.userName |> Option.map ^ UrlBuilder.user
          callback = fun db -> db, () }

    let userTags =
        { url = fun db -> db, db.userName |> Option.map ^ UrlBuilder.user
          callback = fun db -> db, () }

    let topTags =
        { url = fun db -> db, UrlBuilder.home |> Some 
          callback = fun db -> db, () }

    let post id =
        { url = fun db -> db, UrlBuilder.post id |> Some
          callback = fun db -> db, () }

module MergeDomain =
    open Types
    open SyncDomain

    let private filterNotIn target xs =
        let ids =
            target
            |> Seq.map ^ fun x -> x.id
            |> Set.ofSeq
        xs |> Array.filter ^ fun x -> not ^ Set.contains x.id ids

    let mergeApply source =
        let merge (db : LocalDb) =
            let x = Map.tryFind source db.feeds |> Option.defaultValue PostsWithLevels.empty
            let x = { x with
                        old = Array.concat [ x.actual; x.old ] |> filterNotIn x.preloaded
                        actual = x.preloaded
                        preloaded = [||] }
            { db with feeds = Map.add source x db.feeds }
        { url = fun db -> merge db, None
          callback = fun db -> db, () }

    let premergeFirstPage source page =
        let merge (db : LocalDb) =
            Map.tryFind source db.sharedFeeds
            |> Option.map ^ fun (posts, nextPage) ->
                let x = Map.tryFind source db.feeds |> Option.defaultValue PostsWithLevels.empty
                let x =
                    if Seq.isEmpty x.actual
                        then { x with actual = posts; nextPage = nextPage }
                        else { x with preloaded = posts; nextPage = nextPage }
                { db with feeds = Map.add source x db.feeds }
            |> Option.defaultValue db
        { url = fun db -> 
                    { db with sharedFeeds = db.sharedFeeds |> Map.remove source },
                    UrlBuilder.posts source "FIXME" page |> Some
          callback = fun db -> merge db, () }

    let mergeFirstPage source =
        let merge (db : LocalDb) =
            Map.tryFind source db.sharedFeeds
            |> Option.map ^ fun (posts, nextPage) ->
                let x = PostsWithLevels.empty
                let x = { x with
                            actual = Array.concat [ x.actual; filterNotIn x.actual posts ]
                            old = filterNotIn posts x.old
                            nextPage = nextPage }
                { db with feeds = Map.add source x db.feeds }
            |> Option.defaultValue db
        { url = fun db -> 
                    { db with sharedFeeds = db.sharedFeeds |> Map.remove source },
                    UrlBuilder.posts source "FIXME" None |> Some
          callback = fun db -> merge db, () }

    let mergeNextPage source page =
        let merge (db : LocalDb) =
            Map.tryFind source db.sharedFeeds
            |> Option.map ^ fun (posts, nextPage) ->
                let x = Map.tryFind source db.feeds |> Option.defaultValue PostsWithLevels.empty
                let x = { x with
                            actual = Array.concat [ x.actual; filterNotIn x.actual posts ]
                            old = filterNotIn posts x.old
                            nextPage = nextPage }
                { db with feeds = Map.add source x db.feeds }
            |> Option.defaultValue db
        { url = fun db -> 
                    { db with sharedFeeds = db.sharedFeeds |> Map.remove source },
                    UrlBuilder.posts source "FIXME" page |> Some
          callback = fun db -> merge db, () }
