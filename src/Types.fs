module JoyReactor

module Types =
    type Source =
        | FeedSource
        | TagSource of string
        | FavoriteSource

    type Tag =
        { name: string
          image: string }

    type Attachment =
        { url: string
          aspect: float }

    type AttachmentResource =
        { image: Attachment }

    type Comment =
        { text: string
          image: Attachment
          rating: float
          userName: string
          attachments: AttachmentResource [] }

    type Post =
        { id: int
          userName: string
          userImage: Attachment
          rating: double
          created: System.DateTime
          image: Attachment option
          attachments: AttachmentResource []
          title: string
          tags: string []
          comments: Comment [] }

    type PostResponse =
        { posts: Post list
          nextPage: int option }

    type PostsWithLevels =
        { actual: Post []
          old: Post []
          preloaded: Post []
          nextPage: int option }
        static member empty: PostsWithLevels =
            { actual = [||]
              old = [||]
              preloaded = [||]
              nextPage = None }

    type Profile =
        { userName: string
          userImage: Attachment
          rating: float
          stars: int
          progressToNewStar: float }

    type Message =
        { text: string
          date: double
          isMine: bool
          userName: string
          userImage: string }

    [<Fable.Core.Pojo>]
    type MessagesWithNext =
        { messages: Message []
          nextPage: string option }

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

    let posts source userName (page: int option) =
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
    open Types
    open System.Text.RegularExpressions

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

    let filterNewMessages (messages: Message []) offset = messages |> Array.filter (fun x -> x.date > offset)
    let checkMessagesIsOld (messages: Message []) offset = messages |> Array.exists (fun x -> x.date <= offset)

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

    let login (username: string) (password: string) (token: string) =
        let form = Fable.Import.Browser.FormData.Create()
        form.append ("signin[username]", username)
        form.append ("signin[password]", password)
        form.append ("signin[_csrf_token]", token)
        "http://" + UrlBuilder.domain + "/login",
        [ Method HttpMethod.POST
          Credentials RequestCredentials.Sameorigin
          Body !^ form ]
