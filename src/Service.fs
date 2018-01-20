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
    let log msg x =
        printfn "%s" msg
        x

module CommonUi =
    open Fable.Helpers.ReactNative.Props
    open Fable.Helpers.ReactNative
    open Fable.Import.ReactNative

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

    let testButton title f =
        let nextButtonOutter =
            TouchableWithoutFeedbackProperties.Style 
                [ Margin 4. 
                  BackgroundColor "#e49421"
                  BorderRadius 4.
                  Flex 1.
                  Height 48.
                  Overflow Overflow.Hidden ]
        let tabButtonInner =
            TextProperties.Style 
                [ FontWeight FontWeight.Bold
                  FontSize 13.
                  TextAlign TextAlignment.Center
                  Padding 15.
                  Color "white" ]
        touchableOpacity 
            [ nextButtonOutter
              OnPress f ]
            [ text [ tabButtonInner ] title ]

    let myListView<'a> (items: ListViewDataSource<'a>) f =
        listView
            items
            [ ViewProperties.Style [ Flex 1. ]
              ListViewProperties.RenderRow
                  (Func<_,_,_,_,_>(fun (i: 'a) _ _ _ -> f i))
              ListViewProperties.RenderSeparator
                  (Func<_,_,_,_>(fun _ _ _ -> view [ ViewProperties.Style [ Height 1.; BackgroundColor "#f8f8f8" ] ] [])) ]

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
          date: Double
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

    let selectThreads messages = 
        messages
        |> Array.sortByDescending (fun x -> x.date)
        |> Array.distinctBy (fun x -> x.userName)

    let selectMessageForUser userName messages =
        messages
        // |> Array.filter (fun x -> x.userName = userName)
        |> Array.sortByDescending (fun x -> x.date)

    let filterNewMessages (messages: Message[]) offset = 
        messages |> Array.filter (fun x -> x.date > offset)

    let checkMessagesIsOld (messages: Message[]) offset = 
        messages |> Array.exists (fun x -> x.date <= offset)

    let getLastOffsetOrDefault xs =
        xs |> Array.tryMaxBy(fun x -> x.date) 
           |> Option.map (fun x -> x.date) 
           |> Option.defaultValue 0.

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

module Storage =
    open Fable.PowerPack
    open Utils
    let AsyncStorage = Fable.Import.ReactNative.Globals.AsyncStorage
    let JSON = Fable.Import.JS.JSON

    let load<'a> key =
        AsyncStorage.getItem(key)
        |> Promise.map (fun json -> 
            if isNull json then None 
            else json |> (JSON.parse >> unbox<'a>) |> Some)

    let save key value =
        value
        |> JSON.stringify
        |> curry AsyncStorage.setItem key
        |> Promise.map ignore

module Service =
    open JsInterop
    open Fable.PowerPack.Fetch
    open Fable.PowerPack
    open Utils
    open Types

    let loadAllMessageFromStorage =
        Storage.load<Message[]> "messages"
        |> Promise.map (Option.defaultValue [||])

    let loadThreadsFromCache = 
        loadAllMessageFromStorage
        |> Promise.map Domain.selectThreads

    let inline private loadAndParse<'a> parse url = 
        promise {
            let! html =
                url
                |> flip fetch []
                |> Promise.bind (fun response -> response.text())
            let form = Fable.Import.Browser.FormData.Create()
            form.append ("html", html)
            return!
                fetchAs<'a> 
                    (sprintf "http://212.47.229.214:4567/%s" parse)
                    [ Method HttpMethod.POST
                      requestHeaders [ ContentType "multipart/form-data" ]
                      Body !^form ]
        }

    [<Pojo>]
    type MessagesWithNext = { messages: Message[]; nextPage: String option }
    let getMessagesAndNextPage page = 
        UrlBuilder.messages page
        |> loadAndParse<MessagesWithNext> "messages"
        |> Promise.map (fun response -> response.messages, response.nextPage)

    let loadThreadsFromWeb =
        let rec loadPageRec pageNumber lastOffset parentMessages =
            promise {
                let! messages, nextPage = getMessagesAndNextPage pageNumber
                let newMessages = Array.append parentMessages (Domain.filterNewMessages messages lastOffset)
                let flagIsStop = Domain.checkMessagesIsOld messages lastOffset
                if flagIsStop || nextPage.IsNone then return newMessages
                else return! loadPageRec nextPage lastOffset newMessages
            }

        promise {
            let! oldMessages = loadThreadsFromCache
            let lastOffset = Domain.getLastOffsetOrDefault oldMessages
            let! newMessages = loadPageRec None lastOffset [||]

            let messages = newMessages |> Array.append oldMessages

            printfn "Message count = %O" (messages.Length)

            do! Storage.save "messages" messages
            return messages
        }

    let loadMessages username = 
        loadAllMessageFromStorage
        |> Promise.map (Domain.selectMessageForUser username)

    let login username password =
        promise {
            let! tokenOpt =
                fetch "http://joyreactor.cc/ads" []
                |> Promise.bind (fun x -> x.text())
                |> Promise.map Domain.getCsrfToken

            let form = Fable.Import.Browser.FormData.Create ()
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

    let testReloadMessages =
        Fable.Import.ReactNative.Globals.AsyncStorage.clear null
        |> Promise.bind (fun _ -> login "..." "...")
        |> Promise.catch ignore
        |> Promise.bind (fun _ -> loadThreadsFromWeb)
        |> Promise.map ignore

    let loadTags userName =
        UrlBuilder.user userName |> loadAndParse<Tag list> "tags"

    let loadProfile userName =
        UrlBuilder.user userName |> loadAndParse<Profile> "profile"

    let loadPost id =
        UrlBuilder.post id |> loadAndParse<Post> "post"

    let loadPosts source page = 
        UrlBuilder.posts source page 
        |> loadAndParse<PostResponse> "posts"
        |> Promise.map (fun response -> response.posts, response.nextPage)