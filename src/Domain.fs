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
    type LocalDb = JoyReactor.CofxStorage.LocalDb

    type Eff<'a> =    
        { url: LocalDb -> string option
          callback: LocalDb -> 'a }

    let messages page =
        { url = fun _ -> UrlBuilder.messages page |> Some
          callback = fun db -> db.nextMessagesPage }

    let logout =
        { url = fun _ -> sprintf "%s/logout" UrlBuilder.baseUrl |> Some
          callback = ignore }

    let profile =
        { url = fun db -> db.userName |> Option.map ^ UrlBuilder.user
          callback = ignore }

    let userTags =
        { url = fun db -> db.userName |> Option.map ^ UrlBuilder.user
          callback = ignore }

    let topTags =
        { url = fun _ -> UrlBuilder.home |> Some 
          callback = ignore }

    let post id =
        { url = fun _ -> UrlBuilder.post id |> Some
          callback = ignore }

module MergeDomain =
    open Types

    type 'a SyncEffect =
        { uri: string
          api: string
          mkUri: (string -> string option) option
          f: string -> CofxStorage.LocalDb -> Result<CofxStorage.LocalDb * 'a, exn> }

    let inline fromJson<'a> json = try Ok ^ (Fable.Core.JS.JSON.parse >> unbox<'a>) json with e -> Error e

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
