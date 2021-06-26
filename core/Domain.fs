namespace JoyReactor

module UrlBuilder =
    open Types

    let domain = "joyreactor.cc"

    let baseUrl = sprintf "http://%s" domain
    let home = sprintf "%s/" baseUrl
    let donate = sprintf "%s/donate" baseUrl
    let ads = sprintf "%s/ads" baseUrl
    let logout = sprintf "%s/logout" baseUrl

    let messages page =
        page
        |> Option.defaultValue "/private/list"
        |> (+) baseUrl

    let user userName : string =
        System.Uri.EscapeDataString userName
        |> sprintf "%s/user/%s" baseUrl

    let post id = sprintf "%s/post/%i" baseUrl id

    let posts source userName (page: int option) =
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

    let normalize url (w: float) (h: float) =
        sprintf
            "https://rc.y2k.work/cache/fit?width=%i&height=%i&bgColor=ffffff&quality=75&url=%s"
            (int w)
            (int h)
            (Uri.EscapeDataString url)

    let origin url =
        Uri.EscapeDataString url
        |> sprintf "https://rc.y2k.work/cache/origin?url=%s"

    let urlWithHeight limitWidth (attachment: Attachment) =
        let aspect = max 1.2 attachment.aspect
        let w = limitWidth
        let h = w / aspect
        normalize attachment.url w h, h

module UserMessages =
    open Types

    let selectThreads messages =
        messages
        |> Map.toArray
        |> Seq.map snd
        |> Seq.sortByDescending (fun x -> x.date)
        |> Seq.distinctBy (fun x -> x.userName)
        |> Seq.toArray

    let selectMessageForUser userName db =
        db.messages
        |> Map.toSeq
        |> Seq.filter (fun (_, v) -> v.userName = userName)
        |> Seq.map snd
        |> Seq.toArray
        |> Array.sortByDescending (fun x -> x.date)

module FeedMerger =
    open JoyReactor.Types

    let getPostsWithLevels source (feeds: Map<Source, PostsWithLevels>) =
        Map.tryFind source feeds
        |> Option.defaultValue PostsWithLevels.empty

    let mergeFirstPage source sharedFeeds feeds =
        let old = getPostsWithLevels source feeds

        let preloaded =
            sharedFeeds
            |> Option.defaultValue { posts = [||]; nextPage = [||] }

        let a =
            { old with
                  preloaded = preloaded.posts
                  nextPage = preloaded.nextPage |> Array.tryHead }

        Map.add source a feeds, None, a

    let mergePreloaded' source feeds =
        let old = getPostsWithLevels source feeds

        let ids =
            old.preloaded
            |> (Seq.map ^ fun x -> x.id)
            |> Set.ofSeq

        let a =
            { old with
                  preloaded = [||]
                  actual = old.preloaded
                  old =
                      old.actual
                      |> (Seq.filter ^ fun x -> not ^ Seq.contains x.id ids)
                      |> Seq.toArray }

        Map.add source a feeds

    let mergeSecondPage source feeds sharedFeeds =
        let merge old posts =
            Array.concat [ old.actual; posts ]
            |> Array.distinctBy ^ fun x -> x.id

        let mergeOld old posts =
            let keys =
                posts
                |> (Array.map ^ fun x -> x.id)
                |> Set.ofArray

            old.old
            |> Array.filter ^ fun x -> Set.contains x.id keys

        let old = getPostsWithLevels source feeds

        let response =
            sharedFeeds
            |> Option.defaultValue { posts = [||]; nextPage = [||] }

        let a =
            { old with
                  actual = merge old response.posts
                  old = mergeOld old response.posts
                  nextPage = response.nextPage |> Array.tryHead }

        Map.add source a feeds, None

    let replacePosts source sharedFeeds feeds =
        let response =
            sharedFeeds
            |> Option.defaultValue { posts = [||]; nextPage = [||] }

        let a =
            { actual = response.posts
              old = [||]
              preloaded = [||]
              nextPage = response.nextPage |> Array.tryHead }

        Map.add source a feeds, None

module SyncBuilder =
    open JoyReactor.Types
    type private Db = LocalDb

    type Param =
        { pre: Db -> Db
          post: Db -> Db
          url: Db -> Url option }

    let get =
        { pre = id
          post = id
          url = (fun _ -> None) }

    let sync u = { get with url = fun _ -> Some u }
    let withSync u (x: Param) = { x with url = fun _ -> Some u }
    let withSync' fu (x: Param) = { x with url = fun db -> fu db }
    let withPost f (x: Param) = { x with post = f }
