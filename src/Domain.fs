namespace JoyReactor

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
        |> Array.sortByDescending (fun x -> x.date)
        |> Array.distinctBy (fun x -> x.userName)

    let selectMessageForUser userName messages =
        messages
        |> Array.filter (fun x -> x.userName = userName)
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
    open JoyReactor.Types

    type 'a SyncEffect =
        { uri: string
          api: string
          mkUri: (string -> string option) option
          f: string -> CofxStorage.LocalDb -> Result<CofxStorage.LocalDb * 'a, exn> }

    let inline fromJson<'a> json = try Ok ^ (Fable.Core.JS.JSON.parse >> unbox<'a>) json with e -> Error e

    let syncPost id =
        let syncPost id html (db: CofxStorage.LocalDb) =
            fromJson<Post> html
            |> Result.map ^ fun x -> { db with posts = Map.add id x db.posts }, ()
        { uri = (UrlBuilder.post id); api = "post"; mkUri = None; f = syncPost id }

    let syncTopTags =
        let syncTopTags' html (db: CofxStorage.LocalDb) =
            fromJson<Tag []> html
            |> Result.map ^ fun tags -> { db with tags = tags }, ()
        { uri = UrlBuilder.home; api = "toptags"; mkUri = None; f = syncTopTags' }

    let syncMessages page =
        let syncMessages' html (db: CofxStorage.LocalDb) =
            fromJson<MessagesWithNext> html
            |> Result.map ^ fun x ->
                let newMessages, _ = Domain.mergeMessages db.messages x.messages x.nextPage
                { db with messages = newMessages }, x.nextPage
        { uri = UrlBuilder.messages page; api = "messages"; mkUri = None; f = syncMessages' }

    let syncMyProfile =
        let syncMyProfile' html (db: CofxStorage.LocalDb) =
            fromJson<Profile> html
            |> Result.map ^ fun x -> { db with profile = Some x }, ()
        { uri = UrlBuilder.domain; api = "profile"; mkUri = Some Domain.extractName; f = syncMyProfile' }

    let syncTagsWithBackend =
        let syncTagsWithBackend' html (db: CofxStorage.LocalDb) =
            fromJson<Tag []> html
            |> Result.map ^ fun x -> { db with tags = x }, ()
        { uri = UrlBuilder.domain; api = "tags"; mkUri = Some Domain.extractName; f = syncTagsWithBackend' }

module MergeDomain =
    open Types
    open SyncDomain
    open CofxStorage

    let private filterNotIn target xs =
        let ids =
            target
            |> Seq.map ^ fun x -> x.id
            |> Set.ofSeq
        xs |> Array.filter ^ fun x -> not ^ Set.contains x.id ids

    let mergeApply source (db : LocalDb) =
        let x = Map.tryFind source db.feeds |> Option.defaultValue PostsWithLevels.empty
        let x = { x with
                      old = Array.concat [ x.actual; x.old ] |> filterNotIn x.preloaded
                      actual = x.preloaded
                      preloaded = [||] }
        { db with feeds = Map.add source x db.feeds }

    let private genericMerge source page merge =
        let loadPosts' html (db: CofxStorage.LocalDb) =
            fromJson<PostResponse> html
            |> Result.map ^ fun x -> 
                merge (Seq.toArray x.posts) x.nextPage db, ()
        { uri = UrlBuilder.posts source "FIXME" page
          api = "posts"
          mkUri = None
          f = loadPosts' }

    let premergeFirstPage source page =
        let merge posts np (db : LocalDb) =
            let x = Map.tryFind source db.feeds |> Option.defaultValue PostsWithLevels.empty
            let x =
                if Seq.isEmpty x.actual
                    then { x with actual = posts; nextPage = np }
                    else { x with preloaded = posts; nextPage = np }
            { db with feeds = Map.add source x db.feeds }
        genericMerge source page merge

    let mergeNextPage source page =
        let merge posts np (db : LocalDb) =
            let x = Map.tryFind source db.feeds |> Option.defaultValue PostsWithLevels.empty
            let x = { x with
                        actual = Array.concat [ x.actual; filterNotIn x.actual posts ]
                        old = filterNotIn posts x.old
                        nextPage = np }
            { db with feeds = Map.add source x db.feeds }
        genericMerge source page merge
        
    let mergeFirstPage source =
        let merge posts np (db : LocalDb) =
            let x = PostsWithLevels.empty
            let x = { x with
                        actual = Array.concat [ x.actual; filterNotIn x.actual posts ]
                        old = filterNotIn posts x.old
                        nextPage = np }
            { db with feeds = Map.add source x db.feeds }
        genericMerge source None merge
