namespace JoyReactor

open System
open System.Text.RegularExpressions
open Fable.Core

module Array =
    let tryMaxBy f xs =
        try
            xs |> Array.maxBy f |> Some
        with
        | _ -> None

module Utils =
    [<Emit("require($0)")>]
    let require (_: string) = jsNative
    let always a _ = a
    let flip f a b = f b a
    let longToTimeDelay _ = "2 часа"
    let curry f a b = f (a,b)
    let uncurry f (a,b) = f a b

module CommonUi =
    open Fable.Helpers.ReactNative.Props
    open Fable.Helpers.ReactNative

    module private Styles =
        let tabButtonOuter selected = 
            TouchableWithoutFeedbackProperties.Style 
                [ Flex 1.
                  Margin 4. 
                  BackgroundColor (if selected then "#d48411" else "#e49421")
                  BorderRadius 4.
                  Overflow Overflow.Hidden ]
        let tabButtonInner =
            TextProperties.Style 
                [ FontWeight FontWeight.Bold
                  FontSize 13.
                  TextAlign TextAlignment.Center
                  Padding 15.
                  Color "white" ]

    let indicatorView =
        activityIndicator 
            [ ViewProperties.Style [ Flex 1. ]
              ActivityIndicator.Size Size.Large
              ActivityIndicator.Color "#ffb100" ]    

    let viewNavigationBar selected onSelect =
        let button title index = 
            touchableOpacity 
                [ Styles.tabButtonOuter (selected = index)
                  OnPress (fun _ -> onSelect index) ]
                [ text [ Styles.tabButtonInner ] title ]

        view [ ViewProperties.Style [ FlexDirection FlexDirection.Row ] ] 
             [ button "Home" 0
               button "Tags" 1
               button "Messages" 2
               button "Profile" 3 ]

module String =
    let toUpper (x: string) = x.ToUpper()

module Types = 
    type Source =
    | FeedSource
    | TagSource of string

    type Tag = 
        { name: string
          image: string }

    type Attachment = 
        { url : string
          aspect : float }

    type Comment = 
        { text : string
          image : Attachment
          rating : float }

    type Post = 
        { id : int
          userName : string
          userImage: Attachment
          rating : float
          created : System.DateTime
          image : Attachment option
          title : string
          comments : Comment list }

    type PostResponse = 
        { posts : Post list
          nextPage : int option }

    type PostsWithLevels = 
        { actual: Post list
          old: Post list }

    type Profile = 
        { userName: string
          userImage: Attachment
          rating: float
          stars: int
          progressToNewStar: float }

    type Message = 
        { text: String
          date: Int64
          isMine: Boolean
          userName: String
          userImage: String }

module Image =
    open Types
    open Fable.Import.JS

    let normilize url (w : float) (h : float) =
        sprintf
            "http://rc.y2k.work/cache/fit?width=%i&height=%i&bgColor=ffffff&quality=75&url=%s"
            (int w)
            (int h)
            (encodeURIComponent url)

    let urlWithHeight limitWidth (attachment: Attachment) = 
        let aspect = max 1.2 attachment.aspect
        let w = limitWidth
        let h = w / aspect
        normilize attachment.url w h, h

module Domain = 
    open Types

    let mergeNextPage state newPosts = 
        let newActual = 
            newPosts
            |> List.append state.actual
            |> List.distinctBy (fun x -> x.id)
        let newOld = 
            state.old
            |> List.filter (fun x -> List.forall (fun x2 -> x2.id <> x.id) newPosts)
        { actual = newActual; old = newOld }

    let getCsrfToken html = 
        let m = Regex.Match(html, "name=\"signin\\[_csrf_token\\]\" value=\"([^\"]+)")
        if m.Success then Some <| m.Groups.[1].Value else None

    let selectThreads (xs: Message []) = 
        xs
        |> Array.sortBy (fun x -> x.date)
        |> Array.distinctBy (fun x -> x.userName)

    let filterNewMessages (messages: Message[]) offset = 
        messages |> Array.filter (fun x -> x.date > offset)

    let checkMessagesIsOld (messages: Message[]) offset = 
        messages |> Array.exists (fun x -> x.date <= offset)

module UrlBuilder =
    open Fable.Import.JS
    
    let messages page =
        page 
        |> Option.defaultValue "/private/list"
        |> (+) "http://joyreactor.cc"
    
    let user userName =
        encodeURIComponent userName
        |> sprintf "http://joyreactor.cc/user/%s"

    let post id = sprintf "http://joyreactor.cc/post/%i" id

    let posts _ (page: Int32 option) =
        page 
        |> Option.map string 
        |> Option.defaultValue ""
        |> (+) "http://joyreactor.cc/"

module Service =
    open JsInterop
    open Fable.PowerPack.Fetch
    open Fable.PowerPack
    open Utils
    open Types
    module JS = Fable.Import.JS
    let FormData = Fable.Import.Browser.FormData
    let JSON = Fable.Import.JS.JSON
    let AsyncStorage = Fable.Import.ReactNative.Globals.AsyncStorage
    
    let loadThreadsFromCache = 
        AsyncStorage.getItem("messages")
        |> Promise.map (JSON.parse >> unbox<Message []>)
        |> Promise.map (fun x -> if isNull x then [||] else x)
        |> Promise.map Domain.selectThreads

    let private loadAndParse<'a> parse url = 
        promise {
            let! html =
                url
                |> flip fetch []
                |> Promise.bind (fun response -> response.text())
            let form = FormData.Create()
            form.append ("html", html)
            return!
                fetchAs<'a> 
                    (sprintf "http://212.47.229.214:4567/%s" parse)
                    [ Method HttpMethod.POST
                      requestHeaders [ ContentType "multipart/form-data" ]
                      Body !^form ]
        }

    type MessagesWithNext = { message: Message[]; nextPage: String option }
    let getMessagesAndNextPageFromJR (page: String option) = 
        UrlBuilder.messages page
        |> loadAndParse<MessagesWithNext> "messages"
        |> Promise.map (fun response -> response.message, response.nextPage)

    let getLastOffsetOrDefault = 
        loadThreadsFromCache
        |> Promise.map (fun xs -> 
            xs |> Array.tryMaxBy(fun x -> x.date) 
               |> Option.map (fun x -> x.date) 
               |> Option.defaultValue 0L)

    let loadThreadsFromWeb =
        let rec loadPageRec pageNumber lastOffset parentMessages =
            promise {
                let! messages, nextPage = getMessagesAndNextPageFromJR pageNumber
                let newMessages = Array.append parentMessages (Domain.filterNewMessages messages lastOffset)
                let flagIsStop = Domain.checkMessagesIsOld messages lastOffset
                if flagIsStop || nextPage.IsNone then return newMessages
                else return! loadPageRec nextPage lastOffset newMessages
            }

        promise {
            let! lastOffset = getLastOffsetOrDefault
            let! oldMessages = loadThreadsFromCache
            let! newMessages = loadPageRec None lastOffset [||]

            let messages = newMessages |> Array.append oldMessages
            do! messages
                |> JSON.stringify
                |> curry AsyncStorage.setItem ""
                |> Promise.map ignore

            return messages
        }

    let login username password =
        promise {
            let! tokenOpt =
                fetch "http://joyreactor.cc/ads" []
                |> Promise.bind (fun x -> x.text())
                |> Promise.map Domain.getCsrfToken

            let form = FormData.Create ()
            form.append("signin[username]", username)
            form.append("signin[password]", password)
            form.append("signin[_csrf_token]", tokenOpt.Value)

            let! response =
                fetch "http://joyreactor.cc/login" 
                      [ Method HttpMethod.POST
                        Credentials RequestCredentials.Sameorigin
                        Body <| U3.Case2 form ]

            printfn "HEADERS: %O | %O" response.Url response.Headers
        }

    let loadTags userName =
        UrlBuilder.user userName |> loadAndParse<Tag list> "tags"

    let loadProfile userName =
        UrlBuilder.user userName |> loadAndParse<Profile> "profile"

    let loadPost id =
        UrlBuilder.post id |> loadAndParse<Post> "post"

    let loadPosts source page = 
        UrlBuilder.posts source page |> loadAndParse "posts"