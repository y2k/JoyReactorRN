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

open Effects.ReactNative
open JoyReactor.Types

module Storage' =
    open CofxStorage
    open Elmish

    let private refDb = 
        { feeds = Map.empty
          feeds' = Map.empty
          posts = Map.empty
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

let loadPosts source page = async {
    let (Eff x) =
        ApiRequests.parseRequest
            (UrlBuilder.posts source "FIXME" page) // FIXME:
            (fun _ -> None)
            (sprintf "https://%s/%s" UrlBuilder.apiDomain "posts")
        <*> (Fable.Import.JS.JSON.parse >> unbox<PostResponse>)
    let! x = x
    return x.posts, x.nextPage }

let syncNextPage source (state : PostsWithLevels) = async {
    let! (webPosts, nextPage) = loadPosts source state.nextPage
    let actualIds = state.actual |> Array.map (fun x -> x.id)
    let actualPosts = 
        Array.concat [
            state.actual
            webPosts
            |> List.filter (fun x -> not <| Array.contains x.id actualIds)
            |> List.toArray ]
    let ids = actualPosts |> Array.map (fun x -> x.id)
    return
        { actual = actualPosts
          old = state.old |> Array.filter (fun x -> not <| Array.contains x.id ids)
          nextPage = nextPage
          preloaded = [||] } }

let savePostsToCache source (state : PostsWithLevels) =
    Storage'.update ^ fun db -> 
        { db with feeds = Map.add source (Array.concat [ state.actual; state.old ]) db.feeds }, ()

let applyUpdate source state =
    let ids = state.preloaded |> Array.map (fun x -> x.id)
    let newState =
        { state with
            actual = state.preloaded
            old =
                Array.concat [ state.actual; state.old ]
                |> Array.filter (fun x -> not <| Array.contains x.id ids)
            preloaded = [||] }
    let newPosts = Array.concat [ newState.actual; newState.old ]
    Storage'.update ^ fun db -> { db with feeds = Map.add source newPosts db.feeds }, newState

let syncFirstPage source (dbPosts : PostsWithLevels) = async {
    let! (webPosts, nextPage) = loadPosts source None
    return
        match dbPosts.old with
        | [||] -> { PostsWithLevels.empty with
                        actual = webPosts |> List.toArray
                        nextPage = nextPage }
        | _ -> { PostsWithLevels.empty with
                    actual = dbPosts.old
                    preloaded = webPosts |> List.toArray
                    nextPage = nextPage } }

let syncPost id = async {
    let (Eff x) = 
        ApiRequests.parseRequest (UrlBuilder.post id) (fun _ -> None) "post"
        <*> (Fable.Import.JS.JSON.parse >> unbox<Post>)
    let! x = x
    do! Storage'.update ^ fun db -> { db with posts = Map.add id x db.posts }, () }

let login username password =
    ApiRequests.sendForm
        UrlBuilder.ads
        (Domain.getCsrfToken >> Option.get >> Requests.login username password)

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

let syncMessages page = async {
    let (Eff x) = ApiRequests.parseRequest' (UrlBuilder.messages page) (fun _ -> None) "messages"
    let! x = x
    let x = x |> (Fable.Import.JS.JSON.parse >> unbox<MessagesWithNext>)
    return!
        Storage'.update ^ fun db ->
            let newMessages, _ = Domain.mergeMessages db.messages x.messages x.nextPage
            { db with messages = newMessages }, x.nextPage }

let syncTagsWithBackend = async {
    let (Eff x) = 
        ApiRequests.parseRequest
            UrlBuilder.domain
            (Domain.extractName >> Option.get >> UrlBuilder.user >> Some)
            "tags"
        <*> (Fable.Import.JS.JSON.parse >> unbox<Tag []>)
    let! x = x
    do! Storage'.update ^ fun db -> { db with tags = x }, () }
