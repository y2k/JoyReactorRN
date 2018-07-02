namespace JoyReactor

open System
open Fable.Core
open Fable.Helpers.ReactNative

module PromiseOperators =
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f =
        async {
            let! x = ma
            return f x
        }

module Cmd =
    open Elmish
    let ofEffect p f =
        Cmd.ofAsync (fun () -> p) () (Result.Ok >> f) (Result.Error >> f)

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
    let curry f a b = f (a, b)
    let uncurry f (a, b) = f a b
    let log msg x =
        printfn "LOG :: %O" msg
        x
    let trace msg x =
        printfn msg x
        x

module CommonUi =
    open Fable.Helpers.ReactNative.Props

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
                  TextStyle.Color "white" ]

    let testButton title f =
        let nextButtonOutter =
            TouchableWithoutFeedbackProperties.Style 
                [ Margin 4. 
                  BackgroundColor "#e49421"
                  BorderRadius 4.
                  Height 48.
                  Overflow Overflow.Hidden ]
        let tabButtonInner =
            TextProperties.Style 
                [ FontWeight FontWeight.Bold
                  FontSize 13.
                  TextAlign TextAlignment.Center
                  Padding 15.
                  TextStyle.Color "white" ]
        touchableOpacity 
            [ nextButtonOutter
              OnPress f ]
            [ text [ tabButtonInner ] title ]

    let inline myFlatList (items : 'a []) f fid props =
        flatList
            items
            ([ FlatListProperties.KeyExtractor (Func<_,_,_>(fun (i : 'a) _ -> fid i))
               FlatListProperties.RenderItem (Func<_,_>(fun (i : FlatListRenderItemInfo<'a>) -> f i.item)) ]
             @ props)

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

    let statusView status = 
        match status with
        | Some (Ok _) -> view [] []
        | Some (Error _) -> text [] "ERROR"
        | None ->
            activityIndicator 
                [ ViewProperties.Style [ BackgroundColor "#212121"; Padding 4. ]
                  ActivityIndicator.Size Size.Large
                  ActivityIndicator.Color "#ffb100" ]    

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
        { actual : Post []
          old    : Post [] }

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
            "http://rc.y2k.work:8080/cache/fit?width=%i&height=%i&bgColor=ffffff&quality=75&url=%s"
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
    open System.Text.RegularExpressions

    let mergeNextPage state newPosts = 
        let newActual = 
            newPosts
            |> Array.append state.actual
            |> Array.distinctBy (fun x -> x.id)
        let newOld = 
            state.old
            |> Array.filter (fun x -> Array.forall (fun x2 -> x2.id <> x.id) newPosts)
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
        |> Array.filter (fun x -> x.userName = userName)
        |> Array.sortByDescending (fun x -> x.date)

    let filterNewMessages (messages: Message[]) offset = 
        messages |> Array.filter (fun x -> x.date > offset)

    let checkMessagesIsOld (messages: Message[]) offset = 
        messages |> Array.exists (fun x -> x.date <= offset)

    let getLastOffsetOrDefault xs =
        xs |> Array.tryMaxBy (fun x -> x.date) 
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

module Fetch =
    module F = Fable.PowerPack.Fetch
    let fetchString url props =
        async {
            let! r = F.fetch url props |> Async.AwaitPromise
            return! r.text() |> Async.AwaitPromise
        }
    let inline fetchType<'a> url props =
        async {
            return! F.fetchAs<'a> url props |> Async.AwaitPromise
        }

module Requests =
    open JsInterop
    open Fable.PowerPack.Fetch

    let login (username: string) (password: string) (token: string) =
        let form = Fable.Import.Browser.FormData.Create ()
        form.append("signin[username]", username)
        form.append("signin[password]", password)
        form.append("signin[_csrf_token]", token)
        "http://joyreactor.cc/login",
        [ Method HttpMethod.POST
          Credentials RequestCredentials.Sameorigin
          Body !^ form ]

    let parse parseApi (html: string) =
        let form = Fable.Import.Browser.FormData.Create()
        form.append ("html", html)
        (sprintf "https://jrs.y2k.work/%s" parseApi),
        [ Method HttpMethod.POST
          requestHeaders [ ContentType "multipart/form-data" ]
          Body !^ form ]

module Storage =
    open PromiseOperators
    module JS = Fable.Import.JS

    module private AsyncStorage =
        let private _as = Fable.Import.ReactNative.Globals.AsyncStorage
        let setItem key value =
            async {
                let! _ = _as.setItem(key, value) |> Async.AwaitPromise
                return ()
            }
        let getItem key =
            async { return! _as.getItem key |> Async.AwaitPromise }
        let clear =
            async { 
                let! _ = _as.clear () |> Async.AwaitPromise
                return () 
            }

    let inline private tryParse<'a> json =
         if isNull json then None 
         else json |> (JS.JSON.parse >> unbox<'a> >> Some)
    
    let load<'a> key =
        AsyncStorage.getItem key
        >>- tryParse<'a>

    let save key value =
        JS.JSON.stringify value
        |> AsyncStorage.setItem key

    let clear = AsyncStorage.clear

module Service =
    open PromiseOperators
    open Fable.PowerPack.Fetch
    open Fetch
    open Utils
    open Types

    let loadAllMessageFromStorage =
        Storage.load<Message[]> "messages"
        >>- Option.defaultValue [||]

    let loadThreadsFromCache = 
        loadAllMessageFromStorage
        >>- Domain.selectThreads

    let inline private loadAndParse<'a> parseApi url = 
        [ requestHeaders [ HttpRequestHeaders.UserAgent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.1.1 Safari/605.1.15" ] ]
        |> fetchString url
        >>- Requests.parse parseApi
        >>= uncurry fetchType<'a>

    [<Pojo>]
    type MessagesWithNext = { messages: Message[]; nextPage: String option }
    let getMessagesAndNextPage page = 
        UrlBuilder.messages page
        |> loadAndParse<MessagesWithNext> "messages"
        >>- fun response -> response.messages, response.nextPage

    let private syncMessageWithWeb =
        let rec loadPageRec pageNumber parentMessages =
            async {
                let! messages, nextPage = getMessagesAndNextPage pageNumber
                let newMessages, stop = Domain.mergeMessages parentMessages messages nextPage
                return!
                    if stop then async.Return newMessages
                    else loadPageRec nextPage newMessages
            }
        loadAllMessageFromStorage
        >>= loadPageRec None
        >>- trace "Message count = %A"
        >>= Storage.save "messages"

    let loadThreadsFromWeb =
        syncMessageWithWeb
        >>= fun _ -> loadThreadsFromCache

    let loadMessages username = 
        loadAllMessageFromStorage
        >>- Domain.selectMessageForUser username

    let login username password =
        fetchString "http://joyreactor.cc/ads" []
        >>- (Domain.getCsrfToken >> Option.get >> (Requests.login username password))
        >>= uncurry fetchString
        >>- ignore

    let testReloadMessages =
        Storage.clear
        >>= fun _ -> login "..." "..."
        |> Async.Catch 
        >>= fun _ -> loadThreadsFromWeb
        |> Async.Ignore

    open System.Text.RegularExpressions
    let getMyName =
        fetchString "http://joyreactor.cc/donate" []
        >>- fun html -> 
                let m = Regex.Match(html, "<a href=\"/user/([^\"]+)\"\\s+id=\"settings\"")
                if m.Success then Some m.Groups.[1].Value
                else None
        >>- Option.get

    let loadTags userName =
        UrlBuilder.user userName |> loadAndParse<Tag list> "tags"
    let loadMyTags =
        getMyName >>= loadTags

    let loadProfile userName =
        UrlBuilder.user userName |> loadAndParse<Profile> "profile"
    let loadMyProfile =
        getMyName >>= loadProfile

    let loadPost id =
        UrlBuilder.post id |> loadAndParse<Post> "post"

    let loadPosts source page = 
        UrlBuilder.posts source page 
        |> loadAndParse<PostResponse> "posts"
        >>- fun response -> response.posts, response.nextPage

module ReactiveStore =
    open Types
    open Elmish
    open PromiseOperators

    let mutable private postsListener : Dispatch<PostsWithLevels> = ignore

    let listenPostUpdates listener =
        async {        
            postsListener <- listener            

            let! posts = 
                Storage.load<PostsWithLevels> "posts"
                >>- Option.defaultValue { old = [||]; actual = [||] }
            postsListener posts
        } |> Async.StartImmediate

    let syncPosts source page : Async<Int32 option> =
        async {
            let! posts = 
                Storage.load<PostsWithLevels> "posts"
                >>- Option.defaultValue { old = [||]; actual = [||] }

            let! (newPosts, nextPage) =
                Service.loadPosts source page

            let merged = newPosts |> List.toArray |> Domain.mergeNextPage posts
            do! Storage.save "posts" merged

            postsListener merged

            return nextPage
        }

    let reloadPosts =
        async {
            do! Storage.save "posts" { old = [||]; actual = [||] }
            postsListener { old = [||]; actual = [||] }

            return! syncPosts () None
        }

    let mutable private tagListener: Dispatch<Tag []> = ignore

    let listenTagUpdates listener =
        async {
            tagListener <- listener

            let! tags =
                Storage.load<Tag []> "tags"
                >>- Option.defaultValue [||]

            tagListener tags
        } |> Async.StartImmediate

    let syncTags =
        async {
            let! tags = Service.loadMyTags >>- List.toArray

            do! Storage.save "tags" tags

            tagListener tags
        }