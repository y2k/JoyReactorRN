namespace JoyReactor

module UrlBuilder =
    open Types

    let domain = "joyreactor.cc"

    let baseUrl = sprintf "http://%s" domain
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
        sprintf "https://rc.y2k.work/cache/fit?width=%i&height=%i&bgColor=ffffff&quality=75&url=%s" (int w) (int h)
            (Uri.EscapeDataString url)

    let urlWithHeight limitWidth (attachment : Attachment) =
        let aspect = max 1.2 attachment.aspect
        let w = limitWidth
        let h = w / aspect
        normalize attachment.url w h, h

module Domain =
    open Types

    let selectThreads messages =
        messages
        |> Map.toArray
        |> Seq.map snd
        |> Seq.sortByDescending (fun x -> x.date)
        |> Seq.distinctBy (fun x -> x.userName)
        |> Seq.toArray

    let selectMessageForUser' userName (messages : Map<double, Message>) =
        messages
        |> Map.toSeq
        |> Seq.filter (fun (_, v) -> v.userName = userName)
        |> Seq.map snd
        |> Seq.toArray
        |> Array.sortByDescending (fun x -> x.date)

module PostsMergeDomain =
    open JoyReactor.Types

    let getPostsWithLevels source (feeds : Map<Source, PostsWithLevels>) =
        Map.tryFind source feeds
        |> Option.defaultValue PostsWithLevels.empty
    let mergeFirstPage source sharedFeeds feeds =
        let old = getPostsWithLevels source feeds
        let preloaded = sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = [||] }
        let a = { old with preloaded = preloaded.posts; nextPage = preloaded.nextPage |> Array.tryHead }
        Map.add source a feeds, None, a
    let mergePreloaded' source feeds =
        let old = getPostsWithLevels source feeds
        let ids = old.preloaded |> (Seq.map ^ fun x -> x.id) |> Set.ofSeq
        let a = { old with
                    preloaded = [||]
                    actual = old.preloaded
                    old = old.actual |> (Seq.filter ^ fun x -> not ^ Seq.contains x.id ids) |> Seq.toArray }
        Map.add source a feeds
    let mergeSecondPage source feeds sharedFeeds =
        let merge old posts =
            Array.concat [ old.actual; posts ]
            |> Array.distinctBy ^ fun x -> x.id
        let mergeOld old posts =
            let keys = posts |> (Array.map ^ fun x -> x.id) |> Set.ofArray
            old.old |> Array.filter ^ fun x -> Set.contains x.id keys
        let old = getPostsWithLevels source feeds
        let response = sharedFeeds |> Option.defaultValue { posts = [||]; nextPage = [||] }
        let a = { old with 
                    actual = merge old response.posts
                    old = mergeOld old response.posts
                    nextPage = response.nextPage |> Array.tryHead }
        Map.add source a feeds, None
    let replacePosts source sharedFeeds feeds =
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
    let private addMessages messages (newMessages : MessagesWithNext option) =
        match newMessages with
        | Some mesWithNext ->
            mesWithNext.messages
            |> Array.fold (fun a x -> Map.add x.date x a) messages
        | None -> messages
    let private updateNextMessagePage nextMessagesPage newMessages =
        match newMessages with
        | None -> nextMessagesPage
        | Some x -> x.nextPage

    let saveAllParseResults db (pr : ParseResponse) =
        let toMapTag tags dbTags =
            tags 
            |> Option.map ^ fun x -> 
                x 
                |> Array.map (fun x -> x.name, x)
                |> Map.ofArray
            |> Option.defaultValue dbTags
        { db with 
            sharedFeeds = pr.posts
            userName = pr.userName |> Option.orElse db.userName
            userTags = toMapTag pr.userTags db.userTags
            topTags = toMapTag pr.topTags db.topTags
            posts = tryAddPost db.posts pr.post
            profile = pr.profile |> Option.orElse db.profile
            messages2 = addMessages db.messages2 pr.messages
            nextMessagesPage = updateNextMessagePage db.nextMessagesPage pr.messages }

module ActionModule =
    open Elmish
    open JoyReactor
    open JoyReactor.Types
    type Db = CofxStorage.LocalDb

    let mutable downloadAndParseImpl : string -> ParseResponse Async = fun _ -> failwith "not implemented"
    let mutable postFormImpl : PostForm -> ParseResponse Async = fun _ -> failwith "not implemented"

    let private db = ref Db.empty

    let postForm form =
        let invoke =
            async {
                let! pr = postFormImpl form
                db := DomainInterpetator.saveAllParseResults !db pr
                return ()
            }
        Cmd.OfAsync.either (fun _ -> invoke) () Ok Error

    let run (furl: Db -> Db * string option) (callback: Db -> Db * 'a) : Result<'a, exn> Cmd =
        let invoke : _ Async =
            let downloadPostsForUrl url =
                async {
                    let! pr = downloadAndParseImpl url
                    db := DomainInterpetator.saveAllParseResults !db pr
                }
            async {
                let (ldb, opUrl) = furl !db
                db := ldb
                match opUrl with None -> () | Some url -> do! downloadPostsForUrl url
                let (ldb, result) = callback !db
                db := ldb
                return result
            }
        Cmd.OfAsync.either (fun _ -> invoke) () Ok Error

    let readStore (callback: Db -> Db * 'a) : 'a Cmd =
        let invoke =
            async {
                let (ldb, result) = callback !db
                db := ldb
                return result
            }
        Cmd.OfAsync.either (fun _ -> invoke) () id raise

module Services =
    let logout =
        ActionModule.run
            (fun db -> db, Some <| sprintf "%s/logout" UrlBuilder.baseUrl)
            (fun _ -> CofxStorage.LocalDb.empty, ())
    let getMessages userName =
        ActionModule.readStore
            (fun db -> db, Domain.selectMessageForUser' userName db.messages2)
    let getThreads =
        ActionModule.readStore
            (fun db -> 
                db
                , match db.userName with
                  | Some _ -> Some <| Domain.selectThreads db.messages2
                  | None -> None)
    let syncThreads page =
        ActionModule.run
            (fun db -> db, UrlBuilder.messages page |> Some)
            (fun db -> db, (Domain.selectThreads db.messages2, db.nextMessagesPage))
    let profile =
        ActionModule.run
            (fun db -> db, db.userName |> Option.map UrlBuilder.user)
            (fun db -> db, db.profile)
    let private subToTags (db : CofxStorage.LocalDb) = 
        [ db.topTags |> Map.toSeq |> Seq.map snd |> Seq.toArray
          db.userTags  |> Map.toSeq |> Seq.map snd |> Seq.toArray ] 
        |> Array.concat
    let tagFromCache =
        ActionModule.run (fun db -> db, None) (fun db -> db, subToTags db)
    let userTags =
        ActionModule.run 
            (fun db -> db, db.userName |> Option.map ^ UrlBuilder.user) 
            (fun db -> db, subToTags db)
    let topTags = 
        ActionModule.run (fun db -> db, Some UrlBuilder.home) (fun db -> db, subToTags db)
    let postFromCache id =
        ActionModule.run (fun db -> db, None) (fun db -> db, Map.tryFind id db.posts)
    let post id =
        ActionModule.run (fun db -> db, Some ^ UrlBuilder.post id) (fun db -> db, Map.tryFind id db.posts)

module FeedServices =
    open JoyReactor.Types
    open PostsMergeDomain

    let init source =
        ActionModule.run 
            (fun db -> db, None)
            (fun db -> db, getPostsWithLevels source db.feeds)
    let preloadFirstPage source =
        ActionModule.run 
            (fun db -> db, UrlBuilder.posts source "FIXME" None |> Some)
            (fun db ->
                 let (feeds, sharedFeeds, posts) = mergeFirstPage source db.sharedFeeds db.feeds
                 { db with feeds = feeds; sharedFeeds = sharedFeeds }, posts)
    let applyPreloaded source = 
        ActionModule.run 
            (fun db -> db, None)
            (fun db -> 
                 let db = { db with feeds = mergePreloaded' source db.feeds }
                 db, getPostsWithLevels source db.feeds)
    let loadNextPage source = 
        ActionModule.run 
            (fun db ->
                 let a = getPostsWithLevels source db.feeds
                 db, UrlBuilder.posts source "FIXME" a.nextPage |> Some)
            (fun db ->
                 let (feeds, sharedFeeds) = mergeSecondPage source db.feeds db.sharedFeeds
                 let db = { db with feeds = feeds; sharedFeeds = sharedFeeds }
                 db, getPostsWithLevels source db.feeds)
    let refresh source = 
        ActionModule.run 
            (fun db -> db, UrlBuilder.posts source "FIXME" None |> Some)
            (fun db ->
                 let (feeds, sharedFeeds) = replacePosts source db.sharedFeeds db.feeds
                 let db = { db with feeds = feeds; sharedFeeds = sharedFeeds}
                 db, getPostsWithLevels source db.feeds)
