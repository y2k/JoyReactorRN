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
    member __.Bind(x, f) = bind f x
    member __.Return x = Pure x
    member __.ReturnFrom x = x
    member __.Zero() = Pure()

let effect = EffectBuilder()
let fetchText url props = Free(Fetch(url, props, Pure))
let fetchType<'t> url props = Free(FetchJson(typedefof<'t>, url, props, Pure)) |> bind (fun x -> x :?> 't |> Pure)
let loadFromStorage<'t> key: EffectProgram<'t option> = failwith "TODO"
let saveToStorage (key: string) value: EffectProgram<unit> = failwith "TODO"

let fromStorage<'t> key =
    Free(FromStorage(key, Pure))
    |> bind (fun x ->
           x
           |> Option.map (fun x -> x :?> 't)
           |> Pure)

module Storage =
    let load = loadFromStorage
    let save = saveToStorage
    let remove (key: string): EffectProgram<unit> = failwith "TODO"

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

module Cmd =
    let ofEffect e f = Cmd.ofEffect (async { return! Interpreter.run e }) f

module Service =
    open Fable.PowerPack.Fetch
    open Types

    let private fetchApi<'t> url apiName =
        effect {
            let headers =
                [ HttpRequestHeaders.UserAgent
                      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.1.1 Safari/605.1.15" ]
            let! html = fetchText url [ requestHeaders headers ]
            let r = Requests.parse apiName html
            return! r ||> fetchType<'t>
        }

    let loadPost id =
        effect {
            let url = UrlBuilder.post id
            return! fetchApi<Post> url "post"
        }

    let getCachedPosts source =
        effect {
            let! posts = source
                         |> Domain.sourceToString
                         |> fromStorage<Post []>
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

    let getTagsFromCache = effect { let! tags = loadFromStorage<Tag []> "tags"
                                    return Option.defaultValue [||] tags }

    let private loadMyTags =
        effect {
            let! name = getMyName
            let url = Option.map UrlBuilder.user name |> Option.get
            return! fetchApi<Tag list> url "tags"
        }

    let getTagsFromWeb =
        effect {
            let! tags' = loadMyTags
            let tags = List.toArray tags'
            do! saveToStorage "tags" tags
            return tags
        }

    let loadMyProfile =
        effect {
            let! name = getMyName
            let url =
                name
                |> Option.get
                |> UrlBuilder.user
            return! fetchApi<Profile> url "profile"
        }

    let logout = effect { let! _ = fetchText ("http://" + UrlBuilder.domain + "/logout") []
                          return () }
    let private loadAllMessageFromStorage = effect { let! messages = loadFromStorage<Message []> "messages"
                                                     return messages |> Option.defaultValue [||] }
    let loadThreadsFromCache = effect { let! messages = loadAllMessageFromStorage
                                        return messages |> Domain.selectThreads }

    [<Fable.Core.Pojo>]
    type MessagesWithNext =
        { messages: Message []
          nextPage: String option }

    let getMessagesAndNextPage page =
        effect {
            let url = UrlBuilder.messages page
            let! response = fetchApi<MessagesWithNext> url "messages"
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
            do! saveToStorage "messages" messages
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
            let! response = fetchApi<PostResponse> url "posts"
            return response.posts, response.nextPage
        }

    let syncFirstPage source =
        effect {
            let storageId = source |> Domain.sourceToString
            let! x = loadFromStorage<Post []> storageId
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
            do! saveToStorage storageId (Array.concat [ newState.actual;newState.old ])
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
