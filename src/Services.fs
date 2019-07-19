module JoyReactor.Services

module Cmd =
    open Effects.ReactNative
    open Elmish
    let ofEff0 (Eff a) = Cmd.ofSub (fun _ -> Async.StartImmediate a)
    let ofEff f (Eff a) = Cmd.ofAsync (fun _ -> a) () (Ok >> f) (Error >> f)

module private ApiRequests =
    open Effects.ReactNative
    open Fable.Core
    open Fable.PowerPack.Fetch
    open JsInterop

    let private downloadString url props = async {
        let! response = (fetch url props) |> Async.AwaitPromise
        return! response.text() |> Async.AwaitPromise }

    let private props = [ requestHeaders [ UserAgent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.1.1 Safari/605.1.15" ] ]

    type ParseRequest = ParseRequest of url : string * mkUrl : (string -> string option) * parseUrl : string * f : (string -> unit)
    let parseRequest url mkUrl parseUrl =
        async {
            let! html = downloadString url props
            let! html' = match mkUrl html with
                         | Some url -> downloadString url props
                         | None -> async.Return html
            let form = Fable.Import.Browser.FormData.Create()
            form.append ("html", html')
            return! downloadString parseUrl [ Method HttpMethod.POST; Body !^form ]
        } |> Eff.wrap (fun f -> ParseRequest(url, mkUrl, parseUrl, f))
    let parseRequest' url mkUrl path =
        parseRequest url mkUrl (sprintf "https://%s/%s" UrlBuilder.apiDomain path)

    type SendForm = SendForm of csrfUrl : string * mkRequest : (string -> (string * RequestProperties list)) * f : (unit -> unit)
    let sendForm csrfUrl mkRequest =
        let fetchText url props = async {
            let! response = fetch url props |> Async.AwaitPromise
            return! response.text() |> Async.AwaitPromise
        }
        async {
            let! html = fetchText csrfUrl []
            let! _ = mkRequest html ||> fetchText
            ()
        } |> Eff.wrap (fun f -> SendForm(csrfUrl, mkRequest, f))

module private Web =
     open Effects.ReactNative
     open Fable.Core
     open Fable.PowerPack.Fetch

     type DownloadString = DownloadString of url : string * props : RequestProperties list * f : (string -> unit)
     let downloadString url props =
         async {
             let! response = (fetch url props) |> Async.AwaitPromise
             return! response.text() |> Async.AwaitPromise
         } |> Eff.wrap (fun f -> DownloadString(url, props, f))

[<System.Obsolete>]
module private Storage =
    open Effects.ReactNative
    open Fable.Core
    open Fable.Import.JS

    // type IAsyncStorage =
    //     abstract member getItem : string -> string Promise
    //     abstract member removeItem : string -> unit Promise
    //     abstract member setItem : string * string -> unit Promise
    // let private AS : IAsyncStorage = JsInterop.import "AsyncStorage" "react-native"

    // type LoadFromStorage = LoadFromStorage of key : string * f : (string -> unit)
    // let load key =
    //     async { 
    //         return! AS.getItem key |> Async.AwaitPromise }
    //     |> Eff.wrap (fun f -> LoadFromStorage(key, f))

    let inline tryParse<'a> json =
        if isNull json
            then None
            else json |> (Fable.Import.JS.JSON.parse >> unbox<'a> >> Some)

    // let serialize value =
    //     Fable.Import.JS.JSON.stringify value

    // type SaveToStorage = SaveToStorage of key : string * json : string * f : (unit -> unit)
    // let save key json =
    //     async {
    //         if isNull json
    //             then do! AS.removeItem key |> Async.AwaitPromise |> Async.Ignore
    //             else do! AS.setItem (key, json) |> Async.AwaitPromise |> Async.Ignore
    //     }
    //     |> Eff.wrap (fun f -> SaveToStorage(key, json, f))

open Effects.ReactNative
open JoyReactor.Types

module Storage' =
    open CofxStorage
    open Elmish

    let private refDb = 
        { posts = Map.empty
          tags = [||]
          messages = [||]
          profile = None } |> ref

    let private callback : LocalDb Dispatch option ref = ref None

    let sub : LocalDb Cmd =
        Cmd.ofSub ^ fun dispatch ->
            callback := Some dispatch
            dispatch !refDb

    let update f = async {
        let (db, x) = f !refDb
        refDb := db
        match !callback with Some f -> f db | _ -> ()
        return x }
    
    let update' f = update f |> Eff

module Posts =
    let getCachedPosts source =
        // Domain.sourceToString source
        // |> Storage.load
        // <*> Storage.tryParse<Post []>
        // <*> fun posts -> { PostsWithLevels.empty with old = posts |> Option.defaultValue [||] }
        Storage'.update' ^ fun db -> 
            let posts = Map.tryFind source db.posts
            db, { PostsWithLevels.empty with old = posts |> Option.defaultValue [||] }

    let private loadPosts source page =
        ApiRequests.parseRequest
            (UrlBuilder.posts source "FIXME" page) // FIXME:
            (fun _ -> None)
            (sprintf "https://%s/%s" UrlBuilder.apiDomain "posts")
        <*> (Storage.tryParse<PostResponse> >> Option.get)
        <*> (fun response -> response.posts, response.nextPage)

    [<System.Obsolete>]
    let syncFirstPage source (dbPosts : PostsWithLevels) =
        loadPosts source None
        <*> (fun (webPosts, nextPage) ->
                match dbPosts.old with
                | [||] -> { PostsWithLevels.empty with
                                actual = webPosts |> List.toArray
                                nextPage = nextPage }
                | _ -> { PostsWithLevels.empty with
                            actual = dbPosts.old
                            preloaded = webPosts |> List.toArray
                            nextPage = nextPage })

    [<System.Obsolete>]
    let applyUpdate source state =
        let ids = state.preloaded |> Array.map (fun x -> x.id)
        let newState =
            { state with
                actual = state.preloaded
                old =
                    Array.concat [ state.actual; state.old ]
                    |> Array.filter (fun x -> not <| Array.contains x.id ids)
                preloaded = [||] }
        // Array.concat [ newState.actual; newState.old ]
        // |> Storage.serialize
        // |> Storage.save (Domain.sourceToString source)
        // <*> fun _ -> newState
        let newPosts = Array.concat [ newState.actual; newState.old ]
        Storage'.update' ^ fun db -> { db with posts = Map.add source newPosts db.posts }, newState

    [<System.Obsolete>]
    let syncNextPage source (state : PostsWithLevels) =
        loadPosts source state.nextPage
        <*> (fun (webPosts, nextPage) ->
                let actualIds = state.actual |> Array.map (fun x -> x.id)
                let actualPosts = 
                    Array.concat [
                        state.actual
                        webPosts
                        |> List.filter (fun x -> not <| Array.contains x.id actualIds)
                        |> List.toArray ]
                let ids = actualPosts |> Array.map (fun x -> x.id)
                { actual = actualPosts
                  old = state.old |> Array.filter (fun x -> not <| Array.contains x.id ids)
                  nextPage = nextPage
                  preloaded = [||] })

    [<System.Obsolete>]
    let savePostsToCache source (state : PostsWithLevels) =
        // Storage.serialize (Array.concat [ state.actual; state.old ])
        // |> Storage.save (source |> Domain.sourceToString)
        Storage'.update' ^ fun db -> 
            { db with posts = Map.add source (Array.concat [ state.actual; state.old ]) db.posts }, ()

let loadPost id =
    ApiRequests.parseRequest
        (UrlBuilder.post id)
        (fun _ -> None) // post
        "post"
    <*> (Storage.tryParse<Post> >> Option.get)

let login username password =
    ApiRequests.sendForm
        UrlBuilder.ads
        (Domain.getCsrfToken >> Option.get >> Requests.login username password)

[<System.Obsolete>]
let logout' =
    Web.downloadString (sprintf "http://%s/logout" UrlBuilder.domain) []
    <*> ignore

// [<System.Obsolete>]
// let loadMyProfile =
//     ApiRequests.parseRequest
//         UrlBuilder.domain
//         (Domain.extractName >> Option.get >> UrlBuilder.user >> Some)
//         "profile"
//     <*> (Storage.tryParse<Profile> >> Option.get)

let syncMyProfile = async {
    let (Eff x) =
        ApiRequests.parseRequest
            UrlBuilder.domain
            (Domain.extractName >> Option.get >> UrlBuilder.user >> Some)
            "profile"
        <*> (Fable.Import.JS.JSON.parse >> unbox<Profile>)
    let! x = x
    do! Storage'.update ^ fun db -> { db with profile = Some x }, () }

let logout = async {
    let (Eff x) = Web.downloadString (sprintf "http://%s/logout" UrlBuilder.domain) []
    let! _ = x
    return () }

[<System.Obsolete>]
let saveMessageToCache (messages : Message []) =
    // Storage.serialize messages |> Storage.save "messages"
    Storage'.update ^ fun db -> { db with messages = messages }, ()

[<System.Obsolete>]
let loadMessageFromWeb page parentMessages =
    ApiRequests.parseRequest'
        (UrlBuilder.messages page)
        (fun _ -> None)
        "messages"
    <*> (Fable.Import.JS.JSON.parse >> unbox<MessagesWithNext>)
    <*> (fun response ->
            let newMessages, _ = Domain.mergeMessages parentMessages response.messages response.nextPage
            newMessages, response.nextPage)

let syncMessages page = async {
    let (Eff x) = ApiRequests.parseRequest' (UrlBuilder.messages page) (fun _ -> None) "messages"
    let! x = x
    let x = x |> (Fable.Import.JS.JSON.parse >> unbox<MessagesWithNext>)
    return!
        Storage'.update ^ fun db ->
            let newMessages, _ = Domain.mergeMessages db.messages x.messages x.nextPage
            { db with messages = newMessages }, x.nextPage }

[<System.Obsolete>]
let private loadAllMessageFromStorage =
    // Storage.load "messages" <*> (Storage.tryParse<Message []> >> Option.defaultValue [||])
    Storage'.update' ^ fun db -> db, db.messages

// [<System.Obsolete>]
// let loadThreadsFromCache =
//     loadAllMessageFromStorage <*> Domain.selectThreads

// [<System.Obsolete>]
// let loadMessages username =
//     loadAllMessageFromStorage <*> Domain.selectMessageForUser username

// let getTagsFromCache =
//     // Storage.load "tags" <*> (Storage.tryParse<Tag []> >> Option.defaultValue [||])
//     Storage'.update ^ fun db -> db, db.tags

// let saveTagToCache (tags : Tag []) =
//     // Storage.serialize tags |> Storage.save "tags"
//     Storage'.update ^ fun db -> { db with tags = tags }, ()

// let getTagsFromWeb =
//     ApiRequests.parseRequest
//         UrlBuilder.domain
//         (Domain.extractName >> Option.get >> UrlBuilder.user >> Some)
//         (sprintf "https://%s/%s" UrlBuilder.apiDomain "tags")
//     <*> (Storage.tryParse<Tag []> >> Option.get)

let syncTagsWithBackend = async {
    let (Eff x) = 
        ApiRequests.parseRequest
            UrlBuilder.domain
            (Domain.extractName >> Option.get >> UrlBuilder.user >> Some)
            "tags"
        <*> (Fable.Import.JS.JSON.parse >> unbox<Tag []>)
    let! x = x
    do! Storage'.update ^ fun db -> { db with tags = x }, () }
