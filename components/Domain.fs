namespace JoyReactor

module UrlBuilder =
    open Types

    let baseUrl = sprintf "http://%s" UrlBuilder.domain
    let home = sprintf "%s/" baseUrl
    let donate = sprintf "%s/donate" baseUrl
    let ads = sprintf "%s/ads" baseUrl

    let messages page =
        page
        |> Option.defaultValue "/private/list"
        |> (+) baseUrl

    let user userName : string =
        System.Uri.EscapeDataString userName |> sprintf "%s/user/%s" baseUrl
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

module Image =
    open System
    open Types

    let normalize url (w : float) (h : float) =
        sprintf "http://rc.y2k.work:8080/cache/fit?width=%i&height=%i&bgColor=ffffff&quality=75&url=%s" (int w) (int h)
            (Uri.EscapeDataString url)

    let urlWithHeight limitWidth (attachment : Attachment) =
        let aspect = max 1.2 attachment.aspect
        let w = limitWidth
        let h = w / aspect
        normalize attachment.url w h, h

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

// module SyncDomain =
//     type LocalDb = JoyReactor.CofxStorage.LocalDb

//     type Eff<'a> =    
//         { url: LocalDb -> LocalDb * string option
//           callback: LocalDb -> LocalDb * 'a }

//     let private ignore db = db, ()
//     let private fixedUrl url db = db, Some url

//     let messages page =
//         { url = fun db -> 
//               { db with sharedMessages = Set.empty },
//               UrlBuilder.messages page |> Some
//           callback = fun db -> 
//               { db with messages = Set.union db.messages db.sharedMessages },
//               db.nextMessagesPage }
    
//     let logout =
//         { url = fixedUrl ^ sprintf "%s/logout" UrlBuilder.baseUrl
//           callback = ignore }

//     let profile =
//         { url = fun db -> db, db.userName |> Option.map ^ UrlBuilder.user
//           callback = ignore }

//     let userTags =
//         { url = fun db -> db, db.userName |> Option.map ^ UrlBuilder.user
//           callback = ignore }

//     let topTags =
//         { url = fixedUrl UrlBuilder.home 
//           callback = ignore }

module Domain'' =
    open JoyReactor.Types

    let getPostsWithLevels' source (feeds : Map<Source, PostsWithLevels>) =
        Map.tryFind source feeds
        |> Option.defaultValue PostsWithLevels.empty
    let mergeFirstPage' source sharedFeeds feeds =
        let old = getPostsWithLevels' source feeds
        let preloaded = sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = [||] }
        let a = { old with preloaded = preloaded.posts; nextPage = preloaded.nextPage |> Array.tryHead }
        Map.add source a feeds, None, a
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
        let response = sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = [||] }
        let a = { old with 
                    actual = merge old response.posts
                    old = mergeOld old response.posts
                    nextPage = response.nextPage |> Array.tryHead }
        Map.add source a feeds, None
    let replacePosts' source sharedFeeds feeds =
        let response = sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = [||] }
        let a = { actual = response.posts
                  old = [||]
                  preloaded = [||]
                  nextPage = response.nextPage |> Array.tryHead }
        Map.add source a feeds, None              

module DomainInterpetator =
    open JoyReactor.Types
    open JoyReactor.CofxStorage

    let private tryAddPost posts post =
        post
        |> Option.map ^ fun p -> Map.add p.id p posts
        |> Option.defaultValue posts

    let saveAllParseResults db (pr : ParseResponse) =
        let toMapTag tags dbTags =
            tags 
            |> Option.map (fun x -> 
                x 
                |> Array.map (fun x -> x.name, x)
                |> Map.ofArray)
            |> Option.defaultValue dbTags
        { db with 
            sharedFeeds = pr.posts
            userName = pr.userName |> Option.orElse db.userName
            userTags = toMapTag pr.userTags db.userTags
            topTags = toMapTag pr.topTags db.topTags
            posts = tryAddPost db.posts pr.post }
