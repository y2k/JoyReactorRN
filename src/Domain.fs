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

    let private ignore db = db, ()
    let private fixedUrl url db = db, Some url

    let messages page =
        { url = fun db -> 
              { db with sharedMessages = Set.empty },
              UrlBuilder.messages page |> Some
          callback = fun db -> 
              { db with messages = Set.union db.messages db.sharedMessages },
              db.nextMessagesPage }
    
    let logout =
        { url = fixedUrl ^ sprintf "%s/logout" UrlBuilder.baseUrl
          callback = ignore }

    let profile =
        { url = fun db -> db, db.userName |> Option.map ^ UrlBuilder.user
          callback = ignore }

    let userTags =
        { url = fun db -> db, db.userName |> Option.map ^ UrlBuilder.user
          callback = ignore }

    let topTags =
        { url = fixedUrl UrlBuilder.home 
          callback = ignore }

    let post id =
        { url = fixedUrl ^ UrlBuilder.post id
          callback = ignore }

module MergeDomain =
    open Types

    let filterNotIn target xs =
        let ids =
            target
            |> Seq.map ^ fun x -> x.id
            |> Set.ofSeq
        xs |> Array.filter ^ fun x -> not ^ Set.contains x.id ids

    let premergeFirstPage' source sharedFeeds feeds =
        sharedFeeds
        |> Option.map ^ fun { posts = posts; nextPage = nextPage } ->
            let x = Map.tryFind source feeds |> Option.defaultValue PostsWithLevels.empty
            let x =
                if Seq.isEmpty x.actual
                    then { x with actual = posts; nextPage = nextPage |> Array.tryHead }
                    else { x with preloaded = posts; nextPage = nextPage |> Array.tryHead }
            Map.add source x feeds
        |> Option.defaultValue feeds

    let mergeApply' source feeds =
        let x = Map.tryFind source feeds |> Option.defaultValue PostsWithLevels.empty
        let x = { x with
                    old = Array.concat [ x.actual; x.old ] |> filterNotIn x.preloaded
                    actual = x.preloaded
                    preloaded = [||] }
        Map.add source x feeds

    let mergeFirstPage' source sharedFeeds feeds =
        sharedFeeds
        |> Option.map ^ fun { posts = posts; nextPage = nextPage } ->
            let x = PostsWithLevels.empty
            let x = { x with
                        actual = Array.concat [ x.actual; filterNotIn x.actual posts ]
                        old = filterNotIn posts x.old
                        nextPage = nextPage |> Array.tryHead }
            Map.add source x feeds
        |> Option.defaultValue feeds

    let mergeNextPage' source sharedFeeds feeds =
        sharedFeeds
        |> Option.map ^ fun { posts = posts; nextPage = nextPage } ->
            let x = Map.tryFind source feeds |> Option.defaultValue PostsWithLevels.empty
            let x = { x with
                        actual = Array.concat [ x.actual; filterNotIn x.actual posts ]
                        old = filterNotIn posts x.old
                        nextPage = nextPage |> Array.tryHead }
            Map.add source x feeds
        |> Option.defaultValue feeds

module MergeDomainEff =
    open SyncDomain
    open MergeDomain

    let mergeApply source =
        { url = fun db -> { db with feeds = mergeApply' source db.feeds }, None
          callback = fun db -> db, () }

    let premergeFirstPage source page =
        { url = fun db -> 
              { db with sharedFeeds = None }, UrlBuilder.posts source "FIXME" page |> Some
          callback = fun db -> 
              { db with feeds = premergeFirstPage' source db.sharedFeeds db.feeds }, () }

    let mergeFirstPage source =
        { url = fun db -> 
              { db with sharedFeeds = None }, UrlBuilder.posts source "FIXME" None |> Some
          callback = fun db -> 
              { db with feeds = mergeFirstPage' source db.sharedFeeds db.feeds }, () }

    let mergeNextPage source page =
        { url = fun db -> 
              { db with sharedFeeds = None }, UrlBuilder.posts source "FIXME" page |> Some
          callback = fun db -> 
              { db with feeds = mergeNextPage' source db.sharedFeeds db.feeds }, () }
