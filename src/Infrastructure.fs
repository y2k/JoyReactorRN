module JoyReactor.Services

module ApiRequests =
    open Fable.Core
    open Fetch
    open JsInterop
    
    let userAgent = UserAgent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.1.1 Safari/605.1.15"
    let defHeaders' = [ userAgent ]

    let downloadString = ref ^ fun url props headers -> async {
        let! r = async { return! fetch url ((requestHeaders headers) :: props)  |> Async.AwaitPromise } |> Async.Catch
        let response = match r with Choice1Of2 x -> x | Choice2Of2 e -> raise e
        return! response.text() |> Async.AwaitPromise }

    type ParseRequest = ParseRequest of url : string * mkUrl : (string -> string option) * parseUrl : string * f : (string -> unit)
    let parseRequest url mkUrl parseUrl = async {
        let! html = !downloadString url [] defHeaders'
        let! html' = match mkUrl with
                     | Some f -> !downloadString (f html) [] defHeaders'
                     | None -> async.Return html
        return! !downloadString parseUrl [ Method HttpMethod.POST; Body !^ html' ] defHeaders' }

    type SendForm = SendForm of csrfUrl : string * mkRequest : (string -> (string * RequestProperties list)) * f : (unit -> unit)
    let sendForm csrfUrl mkRequest = async {
        Log.log "login 1"
        let! html = !downloadString csrfUrl [] defHeaders'
        Log.log ^ sprintf "login 2 | %O" (mkRequest html)
        let! _ = mkRequest html ||> (fun a b -> !downloadString a b defHeaders')
        Log.log "login 3"
        () }

module EffRuntime =
    open JoyReactor
    open SyncDomain
    module Store = JoyReactor.SyncStore
    
    let private runEffect eff = async {
        let! optUrl = Store.update ^ fun db -> eff.url db
        match optUrl with
        | Some url ->
            let! html = !ApiRequests.downloadString url [] []
            do! Store.update ^ fun db -> { db with parseRequests = db.parseRequests |> Set.add html }, ()
            return! Store.update ^ fun db -> eff.callback db
        | None -> 
            return failwith "No url" }

    let run eff = runEffect eff |> Cmd.ofEffect id

module private Requests =
    open Fable.Core.JsInterop
    open Fetch

    let login (username : string) (password : string) (token : string) =
        let form = Browser.Blob.FormData.Create()
        form.append ("signin[username]", username)
        form.append ("signin[password]", password)
        form.append ("signin[_csrf_token]", token)
        sprintf "%s/login" UrlBuilder.baseUrl,
        [ Method HttpMethod.POST
          requestHeaders [ ContentType "application/x-www-form-urlencoded" ]
          Credentials RequestCredentials.Sameorigin
          Body !^ (string form) ]

let login username password =
    ApiRequests.sendForm
        UrlBuilder.ads
        (Domain.getCsrfToken >> Option.get >> Requests.login username password)
