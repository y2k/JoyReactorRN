module JoyReactor.Services

module Cmd = 
    open Elmish
    open Effects.ReactNative

    let ofEff0 (Eff a) =
        Cmd.ofSub (fun _ -> Async.Start a)

    let ofEff f (Eff a) =
        Cmd.ofAsync (fun _ -> a) () (Ok >> f) (Error >> f)

module ApiRequests =
    open Effects.ReactNative
    open Fable.Core
    open Fable.PowerPack.Fetch
    open JsInterop

    let private downloadString url props =
        async {
            let! response = (fetch url props) |> Async.AwaitPromise
            return! response.text() |> Async.AwaitPromise
        }

    let private props = [ requestHeaders [ HttpRequestHeaders.UserAgent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.1.1 Safari/605.1.15" ] ]

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

module OpenApi =
    open Fable.PowerPack.Fetch
    open Types

    type Replay<'x> = 'x -> unit

    type Html = string

    module Types =
        type ApiRequest =
            | TagListRequest of Html * Replay<Tag list>
            | ProfileRequest of Html * Replay<Profile>
            | PostRequest of Html * Replay<Post>
            | PostsRequest of Html * Replay<PostResponse>
            | MessagesRequest of Html * Replay<MessagesWithNext>

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

module private Storage =
    open Effects.ReactNative
    open Fable.Core

    let private AS = Fable.Import.ReactNative.Globals.AsyncStorage

    type LoadFromStorage = LoadFromStorage of key : string * f : (string -> unit)
    let load key =
        async { return! AS.getItem key |> Async.AwaitPromise }
        |> Eff.wrap (fun f -> LoadFromStorage(key, f))

    let inline tryParse<'a> json =
        if isNull json
            then None
            else json |> (Fable.Import.JS.JSON.parse >> unbox<'a> >> Some)

    let serialize value =
        Fable.Import.JS.JSON.stringify value

    type SaveToStorage = SaveToStorage of key : string * json : string * f : (unit -> unit)
    let save key json =
        async {
            if isNull json
                then do! AS.removeItem key |> Async.AwaitPromise |> Async.Ignore
                else do! AS.setItem (key, json) |> Async.AwaitPromise |> Async.Ignore
        }
        |> Eff.wrap (fun f -> SaveToStorage(key, json, f))

open Effects.ReactNative
open JoyReactor.Types

let getTagsFromCache =
    Storage.load "tags"
    <*> (Storage.tryParse<Tag []> >> Option.defaultValue [||])

let saveTagToCache (tags : Tag []) =
    Storage.serialize tags
    |> Storage.save "tags"

let getTagsFromWeb =
    ApiRequests.parseRequest
        UrlBuilder.domain
        (fun html -> Domain.extractName html |> Option.get |> UrlBuilder.user |> Some)
        (sprintf "https://jrs.y2k.work/%s" "tags")
    <*> (Storage.tryParse<Tag []> >> Option.get)
