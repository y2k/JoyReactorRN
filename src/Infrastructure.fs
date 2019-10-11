module JoyReactor.Services
open Browser
open Fable.React.Props

module Async =
    let unsafe a = async {
        let! x = a
        return match x with Ok x -> x | Error e -> raise e }

module private ApiRequests =
    open Fable.Core
    open Fetch
    open JsInterop

    let private downloadString url props = async {
        let! r = async { return! fetch url props |> Async.AwaitPromise } |> Async.Catch
        let response = match r with Choice1Of2 x -> x | Choice2Of2 e -> raise e
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
        async {
            let! html = downloadString csrfUrl []
            let! _ = mkRequest html ||> downloadString
            () }

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

    let dispatch f = update (fun db -> f db, ())

let runSyncEffect (eff: _ SyncDomain.SyncEffect) =
    ApiRequests.parseRequest eff.uri None (sprintf "%s/%s" UrlBuilder.apiBaseUri eff.api)
    >>= fun html -> Storage.update ^ fun db ->
        match eff.f html db with
        | Ok (newDb, x) -> newDb, Ok x
        | Error e -> db, Error e
    |> Async.unsafe

module private Requests =
    open Fable.Core.JsInterop
    open Fetch

    let login (username : string) (password : string) (token : string) =
        let form = Browser.Blob.FormData.Create()
        form.append ("signin[username]", username)
        form.append ("signin[password]", password)
        form.append ("signin[_csrf_token]", token)
        sprintf "http://%s/login" UrlBuilder.domain,
        [ Method HttpMethod.POST
          Credentials RequestCredentials.Sameorigin
          Body !^ (string form) ]

let login username password =
    ApiRequests.sendForm
        UrlBuilder.ads
        (Domain.getCsrfToken >> Option.get >> Requests.login username password)

module private Web =
     open Fable.Core
     open Fetch

     type DownloadString = DownloadString of url : string * props : RequestProperties list * f : (string -> unit)
     let downloadString url props = async {
         let! response = (fetch url props) |> Async.AwaitPromise
         return! response.text() |> Async.AwaitPromise }

let logout = 
    Web.downloadString (sprintf "http://%s/logout" UrlBuilder.domain) []
    |> Async.Ignore
