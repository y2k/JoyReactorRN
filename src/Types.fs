namespace JoyReactor

module Types =
    type Source =
        | FeedSource
        | TagSource of string
        | FavoriteSource

    type Tag =
        { name : string
          image : string }

    type Attachment =
        { url : string
          aspect : float }

    type AttachmentResource =
        { image : Attachment }

    type Comment =
        { text : string
          image : Attachment
          rating : float
          userName : string
          attachments : AttachmentResource [] }

    type Post =
        { id : int
          userName : string
          userImage : Attachment
          rating : double
          created : System.DateTime
          image : Attachment option
          attachments : AttachmentResource []
          title : string
          tags : string []
          comments : Comment [] }

    type PostResponse =
        { posts : Post list
          nextPage : int option }

    type PostsWithLevels =
        { actual : Post []
          old : Post []
          preloaded : Post []
          nextPage : int option }
        static member empty : PostsWithLevels =
            { actual = [||]
              old = [||]
              preloaded = [||]
              nextPage = None }

    type Profile =
        { userName : string
          userImage : Attachment
          rating : float
          stars : int
          progressToNewStar : float }

    type Message =
        { text : string
          date : double
          isMine : bool
          userName : string
          userImage : string }

    [<Fable.Core.Pojo>]
    type MessagesWithNext =
        { messages : Message []
          nextPage : string option }

module UrlBuilder =
    open Fable.Import.JS
    open Types

    let domain = "joyreactor.cc"
    let donate = "http://" + domain + "/donate"
    let ads = "http://" + domain + "/ads"

    let messages page =
        page
        |> Option.defaultValue "/private/list"
        |> (+) ("http://" + domain)

    let user userName = encodeURIComponent userName |> sprintf "http://%s/user/%s" domain
    let post id = sprintf "http://%s/post/%i" domain id

    let posts source userName (page : int option) =
        match source with
        | FeedSource ->
            page
            |> Option.map string
            |> Option.defaultValue ""
            |> (+) ("http://" + domain + "/")
        | TagSource name ->
            page
            |> Option.map (sprintf "/%i")
            |> Option.defaultValue ""
            |> (+) (sprintf "http://%s/tag/%s" domain name)
        | FavoriteSource ->
            page
            |> Option.map (sprintf "/%i")
            |> Option.defaultValue ""
            |> (+) (sprintf "http://%s/user/%s/favorite" domain userName)

module Domain =
    open System.Text.RegularExpressions
    open Types

    let extractName html =
        let m = Regex.Match(html, "<a href=\"/user/([^\"]+)\"\\s+id=\"settings\"")
        if m.Success then Some m.Groups.[1].Value
        else None

    let sourceToString =
        function
        | FeedSource -> "posts"
        | TagSource name -> "posts-" + name
        | FavoriteSource -> "my-favorites"

    let getCsrfToken html =
        let m = Regex.Match(html, "name=\"signin\\[_csrf_token\\]\" value=\"([^\"]+)")
        if m.Success then Some <| m.Groups.[1].Value
        else None

    let selectThreads messages =
        messages
        |> Array.sortByDescending (fun x -> x.date)
        |> Array.distinctBy (fun x -> x.userName)

    let selectMessageForUser userName messages =
        messages
        |> Array.filter (fun x -> x.userName = userName)
        |> Array.sortByDescending (fun x -> x.date)

    let filterNewMessages (messages : Message []) offset = messages |> Array.filter (fun x -> x.date > offset)
    let checkMessagesIsOld (messages : Message []) offset = messages |> Array.exists (fun x -> x.date <= offset)

    let getLastOffsetOrDefault xs =
        let tryMaxBy f xs =
            try xs |> Array.maxBy f |> Some
            with _ -> None

        xs
        |> tryMaxBy (fun x -> x.date)
        |> Option.map (fun x -> x.date)
        |> Option.defaultValue 0.

    let private isStop messages lastOffset nextPage newMessages =
        let flagIsStop = checkMessagesIsOld messages lastOffset
        flagIsStop || Option.isNone nextPage || Array.length newMessages >= 200

    let mergeMessages parentMessages messages nextPage =
        let lastOffset = getLastOffsetOrDefault parentMessages
        let newMessages = Array.append parentMessages (filterNewMessages messages lastOffset)
        let stop = isStop messages lastOffset nextPage newMessages
        newMessages, stop

module Requests =
    open Fable.Core.JsInterop
    open Fable.PowerPack.Fetch

    let login (username : string) (password : string) (token : string) =
        let form = Fable.Import.Browser.FormData.Create()
        form.append ("signin[username]", username)
        form.append ("signin[password]", password)
        form.append ("signin[_csrf_token]", token)
        "http://" + UrlBuilder.domain + "/login",
        [ Method HttpMethod.POST
          Credentials RequestCredentials.Sameorigin
          Body !^form ]

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
    open Fable.Core
    open System

    let inline ($) f d = f (Fable.Helpers.ReactNative.dip d)
    let inline (<!) f a () = f a
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f = async.Bind(ma, f >> async.Return)

    [<Emit("require($0)")>]
    let require (_ : string) = jsNative

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
    let toUpper (x : string) = x.ToUpper()

module Image =
    open Fable.Import.JS
    open Types

    let normilize url (w : float) (h : float) =
        sprintf "http://rc.y2k.work:8080/cache/fit?width=%i&height=%i&bgColor=ffffff&quality=75&url=%s" (int w) (int h)
            (encodeURIComponent url)

    let urlWithHeight limitWidth (attachment : Attachment) =
        let aspect = max 1.2 attachment.aspect
        let w = limitWidth
        let h = w / aspect
        normilize attachment.url w h, h

module Platform =
    open Fable.Core
    open Fable.Import.ReactNative

    let openUrl url =
        async { do! Globals.Linking.openURL url |> Async.AwaitPromise |> Async.Ignore }