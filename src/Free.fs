module JoyReactor.Free

open System
open Fable.PowerPack.Fetch.Fetch_types

type EffectInstruction<'a> =
    | Fetch of (string * RequestProperties list * (string -> 'a))
    | FromStorage of (string * (obj option -> 'a))
    | SaveToStorage of (string * obj option * (unit -> 'a))
    | ApiRequest of (OpenApi.Replay<obj> -> OpenApi.Types.ApiRequest) * (obj -> 'a)

type EffectProgram<'a> =
    | Free of EffectInstruction<EffectProgram<'a>>
    | Pure of 'a

let rec bind f =
    let mapI f =
        function
        | Fetch(url, props, next) -> Fetch(url, props, next >> f)
        | FromStorage(key, next) -> FromStorage(key, next >> f)
        | SaveToStorage(key, value, next) -> SaveToStorage(key, value, next >> f)
        | ApiRequest(x, next) -> ApiRequest(x, next >> f)
    function
    | Free x ->
        x
        |> mapI (bind f)
        |> Free
    | Pure x -> f x

type EffectBuilder() =
    member __.Bind(x, f) = bind f x
    member __.Return x = Pure x
    member __.ReturnFrom x = x
    member __.Zero() = Pure()

let effect = EffectBuilder()
let fetchText url props = Free(Fetch(url, props, Pure))

module Effects =
    open OpenApi
    open OpenApi.Types

    let apiRequest (f: Replay<'x> -> ApiRequest): EffectProgram<'x> =
        let r = fun (replayObj: Replay<obj>) -> f (fun x -> replayObj x)
        let a = ApiRequest(r, fun x -> x :?> 'x |> Pure)
        Free(a)

module Storage =
    let load<'t> key = Free(FromStorage(key, Pure)) |> bind (fun x -> Pure(x |> Option.map (fun x -> x :?> 't)))
    let save (key: string) value: EffectProgram<unit> = Free(SaveToStorage(key, Some value, Pure))
    let remove (key: string): EffectProgram<unit> = Free(SaveToStorage(key, None, Pure))

module Interpreter =
    module private Storage' =
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

        let load<'a> key = async { let! value = AsyncStorage.getItem key
                                   return tryParse<'a> value }

        let save key =
            function
            | Some value -> JS.JSON.stringify value |> AsyncStorage.setItem key
            | None -> AsyncStorage.setItem key null

    open Fable.Core
    open Fable.PowerPack.Fetch

    let rec run cmd =
        async {
            match cmd with
            | Pure x -> return x
            | Free(Fetch(url, props, next)) -> let! response = fetch url props |> Async.AwaitPromise
                                               let! json = response.text() |> Async.AwaitPromise
                                               return! run (next json)
            | Free(FromStorage(key, next)) -> let! x = Storage'.load key
                                              return! run (next x)
            | Free(SaveToStorage(key, value, next)) ->
                do! Storage'.save key value
                return! run (next())
            | Free(ApiRequest(r, next)) -> let! result = OpenApi.handle r
                                           return! (next >> run) result
        }

module Cmd =
    let ofEffect e f = Cmd.ofEffect (async { return! Interpreter.run e }) f

module Service =
    open Fable.PowerPack.Fetch
    open Types
    open Effects
    open OpenApi.Types

    let private loadHtml url =
        let headers =
            [ HttpRequestHeaders.UserAgent
                  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.1.1 Safari/605.1.15" ]
        fetchText url [ requestHeaders headers ]

    let loadPost id = effect { let! html = UrlBuilder.post id |> loadHtml
                               return! apiRequest (fun x -> PostRequest(html, x)) }

    let getCachedPosts source =
        effect {
            let! posts = source
                         |> Domain.sourceToString
                         |> Storage.load<Post []>
            return { PostsWithLevels.empty with old = posts |> Option.defaultValue [||] }
        }

    let getMyName = effect { let! page = fetchText UrlBuilder.donate []
                             return Domain.extractName page }

    let login username password =
        effect {
            let! page = fetchText UrlBuilder.ads []
            let csrf = Domain.getCsrfToken page
            let r = Requests.login username password (csrf.Value)
            let! _ = r ||> fetchText
            return ()
        }

    let getTagsFromCache = effect { let! tags = Storage.load<Tag []> "tags"
                                    return Option.defaultValue [||] tags }

    let private loadMyTags =
        effect {
            let! name = getMyName
            let url = Option.map UrlBuilder.user name |> Option.get
            let! html = loadHtml url
            return! apiRequest (fun x -> TagListRequest(html, x))
        }

    let getTagsFromWeb =
        effect {
            let! tags' = loadMyTags
            let tags = List.toArray tags'
            do! Storage.save "tags" tags
            return tags
        }

    let loadMyProfile =
        effect {
            let! name = getMyName
            let url =
                name
                |> Option.get
                |> UrlBuilder.user
            let! html = loadHtml url
            return! apiRequest (fun x -> ProfileRequest(html, x))
        }

    let logout = effect { let! _ = fetchText ("http://" + UrlBuilder.domain + "/logout") []
                          return () }
    let private loadAllMessageFromStorage = effect { let! messages = Storage.load<Message []> "messages"
                                                     return messages |> Option.defaultValue [||] }
    let loadThreadsFromCache = effect { let! messages = loadAllMessageFromStorage
                                        return messages |> Domain.selectThreads }

    let getMessagesAndNextPage page =
        effect {
            let url = UrlBuilder.messages page
            let! html = loadHtml url
            let! response = apiRequest (fun x -> MessagesRequest(html, x))
            return response.messages, response.nextPage
        }

    let private syncMessageWithWeb =
        effect {
            let rec loadPageRec pageNumber parentMessages =
                effect {
                    let! messages, nextPage = getMessagesAndNextPage pageNumber
                    let newMessages, stop = Domain.mergeMessages parentMessages messages nextPage
                    return! if stop then Pure newMessages
                            else loadPageRec nextPage newMessages
                }
            let! cachedMessages = loadAllMessageFromStorage
            let! messages = loadPageRec None cachedMessages
            do! Storage.save "messages" messages
        }

    let loadThreadsFromWeb =
        effect {
            do! syncMessageWithWeb
            return! loadThreadsFromCache
        }

    let loadMessages username = effect { let! messages = loadAllMessageFromStorage
                                         return Domain.selectMessageForUser username messages }

    let private loadPosts source page =
        effect {
            let! nameOpt = getMyName
            let name = Option.get nameOpt
            let url = UrlBuilder.posts source name page
            let! html = loadHtml url
            let! response = apiRequest (fun x -> PostsRequest(html, x))
            return response.posts, response.nextPage
        }

    let syncFirstPage source =
        effect {
            let storageId = source |> Domain.sourceToString
            let! x = Storage.load<Post []> storageId
            let dbPosts = x |> Option.defaultValue [||]
            let! (webPosts, nextPage) = loadPosts source None
            let newState =
                match dbPosts with
                | [||] ->
                    { PostsWithLevels.empty with actual = webPosts |> List.toArray
                                                 nextPage = nextPage }
                | _ ->
                    { PostsWithLevels.empty with actual = dbPosts
                                                 preloaded = webPosts |> List.toArray
                                                 nextPage = nextPage }
            do! Storage.save storageId (Array.concat [ newState.actual;newState.old ])
            return newState
        }

    let applyUpdate source state =
        effect {
            let ids = state.preloaded |> Array.map (fun x -> x.id)

            let newState =
                { state with actual = state.preloaded
                             old =
                                 Array.concat [ state.actual;state.old ]
                                 |> Array.filter (fun x -> not <| Array.contains x.id ids)
                             preloaded = [||] }

            let storageId = source |> Domain.sourceToString
            do! Storage.save storageId (Array.concat [ newState.actual;newState.old ])
            return newState
        }

    let reset source =
        effect {
            do! Storage.remove <| Domain.sourceToString source
            return! syncFirstPage source
        }

    let syncNextPage source (state: PostsWithLevels) =
        effect {
            let! (webPosts, nextPage) = loadPosts source state.nextPage
            let actualIds = state.actual |> Array.map (fun x -> x.id)

            let actualPosts =
                Array.concat [ state.actual
                               webPosts
                               |> List.filter (fun x -> not <| Array.contains x.id actualIds)
                               |> List.toArray ]

            let ids = actualPosts |> Array.map (fun x -> x.id)

            let newState =
                { actual = actualPosts
                  old = state.old |> Array.filter (fun x -> not <| Array.contains x.id ids)
                  nextPage = nextPage
                  preloaded = [||] }

            let storageId = source |> Domain.sourceToString
            do! Storage.save storageId (Array.concat [ newState.actual;newState.old ])
            return newState
        }
