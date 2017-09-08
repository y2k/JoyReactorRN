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
      rating : float
      created : System.DateTime
      image : Attachment option
      title : string
      comments : Comment list }

type PostResponse = 
    { posts : Post list
      nextPage : int }

module Service =
    open Fable.Import.Browser
    open Fable.Core.JsInterop
    open Fable.PowerPack.Fetch
    open Fable.PowerPack
    open System.Text.RegularExpressions

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

    let loadPosts _ = 
        promise { 
            let! response = fetch "http://joyreactor.cc/" []
            let! text = response.text()
            let url = "http://212.47.229.214:4567/posts"
            let form = FormData.Create()
            form.append ("html", text)

            let! response = fetchAs<PostResponse> url [ Method HttpMethod.POST
                                                        requestHeaders [ ContentType "multipart/form-data" ]
                                                        Body !^form ]
            return response.posts, Some 0
        }