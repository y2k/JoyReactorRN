module JoyReactor.Free

open System
open Fable.PowerPack.Fetch.Fetch_types

type EffectInstruction<'a> =
    | Fetch of (string * RequestProperties list * (string -> 'a))
    | FetchJson of (Type * string * RequestProperties list * (obj -> 'a))
    | FromStorage of (string * (obj option -> 'a))

type EffectProgram<'a> =
    | Free of EffectInstruction<EffectProgram<'a>>
    | Pure of 'a

let rec bind f =
    let mapI f =
        function 
        | Fetch(url, props, next) -> Fetch(url, props, next >> f)
        | FetchJson(t, url, props, next) -> FetchJson(t, url, props, next >> f)
        | FromStorage(key, next) -> FromStorage(key, next >> f)
    function 
    | Free x -> 
        x
        |> mapI (bind f)
        |> Free
    | Pure x -> f x

type EffectBuilder() =
    member this.Bind(x, f) = bind f x
    member this.Return x = Pure x
    member this.ReturnFrom x = x
    member this.Zero() = Pure()

let effect = EffectBuilder()
let fetchText url props = Free(Fetch(url, props, Pure))
let fetchType<'t> url props = Free(FetchJson(typedefof<'t>, url, props, Pure)) |> bind (fun x -> x :?> 't |> Pure)

let fromStorage<'t> key =
    Free(FromStorage(key, Pure))
    |> bind (fun x -> 
           x
           |> Option.map (fun x -> x :?> 't)
           |> Pure)

module Interpreter =
    module Storage' =
        open Fable.Core
        
        module JS = Fable.Import.JS
        
        module private AsyncStorage =
            let private _as = Fable.Import.ReactNative.Globals.AsyncStorage
            let setItem key value = async { let! _ = _as.setItem (key, value) |> Async.AwaitPromise
                                            return () }
            let getItem key = async { return! _as.getItem key |> Async.AwaitPromise }
        
        let inline private tryParse<'a> json =
            if isNull json then None
            else 
                json
                |> (JS.JSON.parse
                    >> unbox<'a>
                    >> Some)
        
        let load<'a> key = AsyncStorage.getItem key >>- tryParse<'a>
    
    open Fable.Core
    open Fable.PowerPack.Fetch
    
    let rec run cmd =
        async { 
            match cmd with
            | Pure x -> return x
            | Free(Fetch(url, props, next)) -> let! response = fetch url props |> Async.AwaitPromise
                                               let! json = response.text() |> Async.AwaitPromise
                                               return! run (next json)
            | Free(FetchJson(t, url, props, next)) -> 
                let! response = fetchAs url props |> Async.AwaitPromise
                let x = response
                return! run (next x)
            | Free(FromStorage(key, next)) -> let! x = Storage'.load key
                                              return! run (next x)
        }

module Service =
    open Fable.PowerPack.Fetch
    open Types
    
    let private fetchApi<'t> url =
        effect { 
            let headers =
                [ HttpRequestHeaders.UserAgent 
                      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.1.1 Safari/605.1.15" ]
            return! fetchType<'t> url [ requestHeaders headers ]
        }
    
    let loadPost id =
        effect { 
            let url = UrlBuilder.post id
            return! fetchApi<Post> url
        }
    
    let getCachedPosts source =
        effect { 
            let! posts = source
                         |> Domain.sourceToString
                         |> fromStorage<Post []>
            return { PostsWithLevels.empty with old = posts |> Option.defaultValue [||] }
        }
    
    let getMyName = effect { let! page = fetchText "http://joyreactor.cc/donate" []
                             return Domain.extractName page }
    
    let loadMyTags =
        effect { 
            let! name = getMyName
            let url = Option.map UrlBuilder.user name |> Option.get
            return! fetchApi<Tag list> url
        }
    
    let login username password =
        effect { 
            let! page = fetchText "http://joyreactor.cc/ads" []
            let csrf = Domain.getCsrfToken page
            let r = Requests.login username password (csrf.Value)
            let! _ = r ||> fetchText
            ()
        }
