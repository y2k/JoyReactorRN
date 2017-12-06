namespace JoyReactor

open System.Text.RegularExpressions
open Fable.Import.Browser
open Fable.Core

module String =
    let toUpper (x: string) = x.ToUpper()

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

module Utils =
    let flip f a b = f b a

module Image =
    let normilize url (w : float) (h : float) =
        sprintf
            "http://rc.y2k.work/cache/fit?width=%i&height=%i&bgColor=ffffff&quality=75&url=%s"
            (int w)
            (int h)
            (Fable.Import.JS.encodeURIComponent url)

    let urlWithHeight limitWidth attachment = 
        let aspect = max 1.2 attachment.aspect
        let w = limitWidth
        let h = w / aspect
        normilize attachment.url w h, h

module Domain = 
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

module Service =
    open JsInterop
    open Fable.PowerPack.Fetch
    open Fable.PowerPack
    open Fable.Import.JS
    module B = Fable.Import.Browser
    module P = Fable.PowerPack.Promise

    let login username password =
        promise {
            let! tokenOpt =
                fetch "http://joyreactor.cc/ads" []
                |> P.bind (fun x -> x.text())
                |> P.map Domain.getCsrfToken

            let form = FormData.Create ()
            form.append("signin[username]", username)
            form.append("signin[password]", password)
            form.append("signin[_csrf_token]", tokenOpt.Value)

            let form2 = 
                sprintf 
                    "signin[username]=%s&signin[password]=%ssignin[_csrf_token]=%s"
                    username password tokenOpt.Value

            let request = XMLHttpRequest.Create()
            request.``open``("POST", "http://joyreactor.cc/login")
            request.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")
            request.addEventListener_readystatechange (
                fun state ->
                    printfn "STATE :: %O" (request.getAllResponseHeaders())
                    obj())
            request.send(Some form2)

            let! response =
                fetch "http://joyreactor.cc/login" 
                      [ Method HttpMethod.POST
                        Credentials RequestCredentials.Sameorigin
                        Body <| U3.Case2 form ]

            printfn "HEADERS: %O | %O" response.Url response.Headers
        }

    let loadTags userName =
        promise {
            let encodedUserName = encodeURIComponent userName
            let! response = fetch (sprintf "http://joyreactor.cc/user/%s" encodedUserName) []
            let! text = response.text()
            let url = "http://212.47.229.214:4567/tags"
            let form = FormData.Create()
            form.append ("html", text)

            let! response = fetchAs<Tag list> url [ Method HttpMethod.POST
                                                    requestHeaders [ ContentType "multipart/form-data" ]
                                                    Body !^form ]
            return response
        }

    let loadProfile userName =
        promise {
            let encodedUserName = encodeURIComponent userName
            let! response = fetch (sprintf "http://joyreactor.cc/user/%s" encodedUserName) []
            let! text = response.text()
            let url = "http://212.47.229.214:4567/profile"
            let form = FormData.Create()
            form.append ("html", text)

            let! response = fetchAs<Profile> url [ Method HttpMethod.POST
                                                   requestHeaders [ ContentType "multipart/form-data" ]
                                                   Body !^form ]
            return response
        }

    let loadPost id =
        promise {
            let! response = fetch (sprintf "http://joyreactor.cc/post/%i" id) []
            let! text = response.text()
            let url = "http://212.47.229.214:4567/post"
            let form = FormData.Create()
            form.append ("html", text)

            let! response = fetchAs<Post> url [ Method HttpMethod.POST
                                                requestHeaders [ ContentType "multipart/form-data" ]
                                                Body !^form ]
            return response
        }

    let loadPosts source page = 
        promise { 
            let sp = match page with | Some (x : int) -> string x | _ -> ""
            let! response = fetch ("http://joyreactor.cc/" + sp) []
            let! text = response.text()
            let url = "http://212.47.229.214:4567/posts"
            let form = FormData.Create()
            form.append ("html", text)

            let! response = fetchAs<PostResponse> url [ Method HttpMethod.POST
                                                        requestHeaders [ ContentType "multipart/form-data" ]
                                                        Body !^form ]
            return response.posts, response.nextPage
        }