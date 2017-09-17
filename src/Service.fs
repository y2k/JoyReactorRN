namespace JoyReactor

type Source =
| FeedSource
| TagSource of string

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

type PostsWithLevels = {
    actual: Post list
    old: Post list
}

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

module Service =
    open System.Text.RegularExpressions
    open Fable.Core.JsInterop
    open Fable.PowerPack.Fetch
    open Fable.PowerPack
    module B = Fable.Import.Browser

    let loadPost id =
        promise {
            let! response = fetch (sprintf "http://joyreactor.cc/post/%i" id) []
            let! text = response.text()
            let url = "http://212.47.229.214:4567/post"
            let form = B.FormData.Create()
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
            let form = B.FormData.Create()
            form.append ("html", text)

            let! response = fetchAs<PostResponse> url [ Method HttpMethod.POST
                                                        requestHeaders [ ContentType "multipart/form-data" ]
                                                        Body !^form ]
            return response.posts, response.nextPage
        }