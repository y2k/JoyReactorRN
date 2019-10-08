module JoyReactor.Services

module private ApiRequests =
    open Fable.Core
    open Fetch
    open JsInterop

    let private downloadString url props = async {
        let! response = (fetch url props) |> Async.AwaitPromise
        return! response.text() |> Async.AwaitPromise }

    let private props = [ requestHeaders [ UserAgent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.1.1 Safari/605.1.15" ] ]

    type ParseRequest = ParseRequest of url : string * mkUrl : (string -> string option) * parseUrl : string * f : (string -> unit)
    let parseRequest url mkUrl parseUrl = async {
        let! html = downloadString url props
        let! html' = match mkUrl with
                     | Some f -> downloadString (f html) props
                     | None -> async.Return html
        return! downloadString parseUrl [ Method HttpMethod.POST; Body !^ html' ] }

    type SendForm = SendForm of csrfUrl : string * mkRequest : (string -> (string * RequestProperties list)) * f : (unit -> unit)
    let sendForm csrfUrl mkRequest =
        let fetchText url props = async {
            let! response = fetch url props |> Async.AwaitPromise
            return! response.text() |> Async.AwaitPromise }
        async {
            let! html = fetchText csrfUrl []
            let! _ = mkRequest html ||> fetchText
            ()
        }

module private Web =
     open Fable.Core
     open Fetch

     type DownloadString = DownloadString of url : string * props : RequestProperties list * f : (string -> unit)
     let downloadString url props = async {
         let! response = (fetch url props) |> Async.AwaitPromise
         return! response.text() |> Async.AwaitPromise }

module Storage =
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

open JoyReactor.Types

let inline parseObj<'a> = Fable.Core.JS.JSON.parse >> unbox<'a>

let loadPosts source page = async {
    let! x =
        ApiRequests.parseRequest
            (UrlBuilder.posts source "FIXME" page) // FIXME:
            None
            (sprintf "%s/%s" UrlBuilder.apiBaseUri "posts")
        >>- parseObj<PostResponse>
    return x.posts, x.nextPage }

let syncPost id = async {
    let! x = 
        ApiRequests.parseRequest (UrlBuilder.post id) None "post"
        >>- parseObj<Post>
    do! Storage.update ^ fun db -> { db with posts = Map.add id x db.posts }, () }

let login username password =
    ApiRequests.sendForm
        UrlBuilder.ads
        (Domain.getCsrfToken >> Option.get >> Requests.login username password)

let syncMyProfile = async {
    let! x =
        ApiRequests.parseRequest
            UrlBuilder.domain
            (Some (Domain.extractName >> Option.get))
            "profile"
        >>- parseObj<Profile>
    do! Storage.update ^ fun db -> { db with profile = Some x }, () }

let logout = 
    Web.downloadString (sprintf "http://%s/logout" UrlBuilder.domain) []
    |> Async.Ignore

let syncMessages page = async {
    let! x = 
        ApiRequests.parseRequest (UrlBuilder.messages page) None "messages"
        >>- parseObj<MessagesWithNext>
    return!
        Storage.update ^ fun db ->
            let newMessages, _ = Domain.mergeMessages db.messages x.messages x.nextPage
            { db with messages = newMessages }, x.nextPage }

let syncTagsWithBackend = async {
    let! x = 
        ApiRequests.parseRequest
            UrlBuilder.domain
            (Some (Domain.extractName >> Option.get))
            "tags"
        >>- parseObj<Tag []>
    do! Storage.update ^ fun db -> { db with tags = x }, () }
