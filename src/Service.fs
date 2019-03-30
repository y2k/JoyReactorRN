namespace JoyReactor

open System
open Fable.Core
open Fable.Helpers.ReactNative
open Fable.Import.ReactNative

module Cmd =
    open Elmish

    let ofEffect p f = Cmd.ofAsync (fun () -> p) () (Result.Ok >> f) (Result.Error >> f)
    let ofEffect0 p = Cmd.ofSub (fun _ -> p |> Async.StartImmediate)

module Array =
    let tryMaxBy f xs =
        try xs |> Array.maxBy f |> Some
        with _ -> None

[<AutoOpen>]
module Utils =
    let inline (<!) f a () = f a
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f = async.Bind(ma, f >> async.Return)

    [<Emit("require($0)")>]
    let require (_: string) = jsNative

    let inline always a _ = a
    let inline flip f a b = f b a
    let longToTimeDelay _ = "2 часа"
    let inline curry f a b = f (a, b)
    let inline uncurry f (a, b) = f a b
    let mutable private startTime = DateTime.Now.Ticks / 10_000L

    let log msg x =
        let delay = DateTime.Now.Ticks / 10_000L - startTime
        printfn "LOGX (%O) :: %O" delay msg
        startTime <- DateTime.Now.Ticks / 10_000L
        x

    let trace msg x =
        printfn msg x
        x

module String =
    let toUpper (x: string) = x.ToUpper()

module Image =
    open Types
    open Fable.Import.JS

    let normilize url (w: float) (h: float) =
        sprintf "http://rc.y2k.work:8080/cache/fit?width=%i&height=%i&bgColor=ffffff&quality=75&url=%s" (int w) (int h)
            (encodeURIComponent url)

    let urlWithHeight limitWidth (attachment: Attachment) =
        let aspect = max 1.2 attachment.aspect
        let w = limitWidth
        let h = w / aspect
        normilize attachment.url w h, h

module Platform =
    open Fable.Import.ReactNative

    let openUrl url = async { let! _ = Globals.Linking.openURL url |> Async.AwaitPromise
                              return () }

module Fetch =
    module F = Fable.PowerPack.Fetch

    let fetchString url props = async { let! r = F.fetch url props |> Async.AwaitPromise
                                        return! r.text() |> Async.AwaitPromise }

module Requests =
    open JsInterop
    open Fable.PowerPack.Fetch

    let login (username: string) (password: string) (token: string) =
        let form = Fable.Import.Browser.FormData.Create()
        form.append ("signin[username]", username)
        form.append ("signin[password]", password)
        form.append ("signin[_csrf_token]", token)
        "http://" + UrlBuilder.domain + "/login",
        [ Method HttpMethod.POST
          Credentials RequestCredentials.Sameorigin
          Body !^form ]

module OpenApi =
    open Types
    open JsInterop
    open Fable.PowerPack.Fetch

    type Replay<'x> = 'x -> unit

    type Html = string

    module Types =
        type ApiRequest =
            | TagListRequest of Html * Replay<Tag list>
            | ProfileRequest of Html * Replay<Profile>
            | PostRequest of Html * Replay<Post>
            | PostsRequest of Html * Replay<PostResponse>
            | MessagesRequest of Html * Replay<MessagesWithNext>

    open Types

    module F = Fable.PowerPack.Fetch

    let inline private fetchType<'a> url props = async { return! F.fetchAs<'a> url props |> Async.AwaitPromise }

    let private parse parseApi (html: string) =
        let form = Fable.Import.Browser.FormData.Create()
        form.append ("html", html)
        (sprintf "https://jrs.y2k.work/%s" parseApi),
        [ Method HttpMethod.POST
          Body !^form ]

    let handle (f: Replay<'x> -> ApiRequest): Async<'x> =
        async {
            let mutable result: 'x option = None
            let a = f (fun x -> result <- Some x)
            match a with
            | TagListRequest(html, g) ->
                let r = parse "tags" html
                let! x = r ||> fetchType
                g x
            | ProfileRequest(html, g) ->
                let r = parse "profile" html
                let! x = r ||> fetchType
                g x
            | PostRequest(html, g) ->
                let r = parse "post" html
                let! x = r ||> fetchType
                g x
            | PostsRequest(html, g) ->
                let r = parse "posts" html
                let! x = r ||> fetchType
                g x
            | MessagesRequest(html, g) ->
                let r = parse "messages" html
                let! x = r ||> fetchType
                g x
            return result.Value
        }
