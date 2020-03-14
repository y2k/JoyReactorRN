namespace JoyReactor

open CofxStorage

module UrlBuilder =
    // open Fable.Core.JS
    open Types

    let baseUrl = sprintf "http://%s" UrlBuilder.domain
    let home = sprintf "%s/" baseUrl
    let donate = sprintf "%s/donate" baseUrl
    let ads = sprintf "%s/ads" baseUrl

    let messages page =
        page
        |> Option.defaultValue "/private/list"
        |> (+) baseUrl

    let user userName : string = failwith "FIXME"
        // encodeURIComponent userName |> sprintf "%s/user/%s" baseUrl
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

module Domain'' =
    open JoyReactor.Types

    let getPostsWithLevels' source (feeds : Map<Source, PostsWithLevels>) =
        Map.tryFind source feeds
        |> Option.defaultValue PostsWithLevels.empty
    let mergeFirstPage' source sharedFeeds feeds =
        let old = getPostsWithLevels' source feeds
        let preloaded = sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = None }
        let a = { old with preloaded = preloaded.posts; nextPage = preloaded.nextPage }
        Map.add source a feeds, None
    let mergePreloaded' source feeds =
        let old = getPostsWithLevels' source feeds
        let ids = old.preloaded |> (Seq.map ^ fun x -> x.id) |> Set.ofSeq
        let a = { old with
                    preloaded = [||]
                    actual = old.preloaded
                    old = old.actual |> (Seq.filter ^ fun x -> not ^ Seq.contains x.id ids) |> Seq.toArray }
        Map.add source a feeds
    let mergeSecondPage' source feeds sharedFeeds =
        let merge old posts =
            Array.concat [ old.actual; posts ]
            |> Array.distinctBy ^ fun x -> x.id
        let mergeOld old posts =
            let keys = posts |> (Array.map ^ fun x -> x.id) |> Set.ofArray
            old.old |> Array.filter ^ fun x -> Set.contains x.id keys

        let old = getPostsWithLevels' source feeds
        let response = sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = None }
        let a = { old with 
                    actual = merge old response.posts
                    old = mergeOld old response.posts
                    nextPage = response.nextPage }
        Map.add source a feeds, None
    let replacePosts' source sharedFeeds feeds =
        let response = sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = None }
        let a = { actual = response.posts
                  old = [||]
                  preloaded = [||]
                  nextPage = response.nextPage }
        Map.add source a feeds, None              

module Domain' =
    open JoyReactor
    open JoyReactor.Types
    open SyncDomain
    open Domain''

    let init source =
        { url = fun db -> db, None
          callback = fun db -> db, getPostsWithLevels' source db.feeds }
    let preloadFirstPage source =
        { url = fun db -> db, UrlBuilder.posts source "FIXME" None |> Some
          callback = fun db ->
              let (feeds, sharedFeeds) = mergeFirstPage' source db.sharedFeeds db.feeds
              { db with feeds = feeds; sharedFeeds = sharedFeeds }, () }
    let applyPreloaded source = 
        { url = fun db -> db, None
          callback = fun db -> 
              let db = { db with feeds = mergePreloaded' source db.feeds }
              db, getPostsWithLevels' source db.feeds }
    let loadNextPage source = 
        { url = fun db ->
            let a = getPostsWithLevels' source db.feeds
            db, UrlBuilder.posts source "FIXME" a.nextPage |> Some
          callback = fun db ->
              let (feeds, sharedFeeds) = mergeSecondPage' source db.feeds db.sharedFeeds
              let db = { db with feeds = feeds; sharedFeeds = sharedFeeds }
              db, getPostsWithLevels' source db.feeds }
    let refresh source = 
        { url = fun db -> db, UrlBuilder.posts source "FIXME" None |> Some
          callback = fun db ->
              let (feeds, sharedFeeds) = replacePosts' source db.sharedFeeds db.feeds
              let db = { db with feeds = feeds; sharedFeeds = sharedFeeds }
              db, getPostsWithLevels' source db.feeds }

module FeedDomainComponent =
    open JoyReactor.Types
    open Domain''

    type 'a Msg =
        | InitAction of Source * (Result<PostsWithLevels, exn> -> 'a)
        | PreloadFirstPageAction of Source * (Result<unit, exn> -> 'a)
        | ApplyPreloadedAction of Source * (Result<PostsWithLevels, exn> -> 'a)
        | LoadNextPageAction of Source * (Result<PostsWithLevels, exn> -> 'a)
        | RefreshAction of Source * (Result<PostsWithLevels, exn> -> 'a)
        | PreloadFirstPageActionEnd of Source * PostResponse * (Result<unit, exn> -> 'a)
        | LoadNextPageActionEnd of Source * PostResponse * (Result<PostsWithLevels, exn> -> 'a)
        | RefreshActionEnd of Source * PostResponse * (Result<PostsWithLevels, exn> -> 'a)

    type 'a Action =
        | SetResultAction of 'a
        | DownloadPostsAction of string * (PostResponse -> 'a Msg)

    let update (model : LocalDb) msg =
        match msg with 
        | InitAction (source, f) -> 
            model
            , getPostsWithLevels' source model.feeds |> Ok |> f |> SetResultAction
        | PreloadFirstPageAction (source, f) -> 
            let url = UrlBuilder.posts source "FIXME" None
            model, DownloadPostsAction (url, fun p -> PreloadFirstPageActionEnd (source, p, f))
        | PreloadFirstPageActionEnd (source, posts, f) ->
            let (feeds, sharedFeeds) = mergeFirstPage' source (Some posts) model.feeds
            { model with feeds = feeds; sharedFeeds = sharedFeeds }
            , Ok () |> f |> SetResultAction
        | ApplyPreloadedAction (source, f) ->
            let model = { model with feeds = mergePreloaded' source model.feeds }
            model, getPostsWithLevels' source model.feeds |> Ok |> SetResultAction
        | LoadNextPageAction (source, f) ->
            let a = getPostsWithLevels' source model.feeds
            let url = UrlBuilder.posts source "FIXME" a.nextPage
            model, DownloadPostsAction (url, fun p -> LoadNextPageActionEnd (source, p, f))
        | LoadNextPageActionEnd (source, posts, f) ->
            let (feeds, sharedFeeds) = mergeSecondPage' source model.feeds (Some posts)
            let model = { model with feeds = feeds; sharedFeeds = sharedFeeds }
            model, getPostsWithLevels' source model.feeds |> Ok |> f |> SetResultAction
        | RefreshAction (source, f) ->
            let url = UrlBuilder.posts source "FIXME" None
            model, DownloadPostsAction (url, fun p -> RefreshActionEnd (source, p, f))
        | RefreshActionEnd (source, posts, f) ->
            let (feeds, sharedFeeds) = replacePosts' source (Some posts) model.feeds
            let model = { model with feeds = feeds; sharedFeeds = sharedFeeds }
            model, getPostsWithLevels' source model.feeds |> Ok |> f |> SetResultAction
