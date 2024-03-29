﻿namespace JoyReactor

module TagResolver =
    type Storage = { names : int []; icons : int [] }

    let private hashCode (value : string) =
        value |> Seq.fold (fun h i -> 31 * h + (int i)) 0

    let tryGetImageId (storage : Storage) (name : string) : int option =
        let index = System.Array.BinarySearch(storage.names, hashCode name)
        if index >= 0 then storage.icons.[index] |> Some else None

    let inline (!!) x = if isNull x then raise <| System.NullReferenceException() else x

    let private readInts path =
        let input = !!(typeof<Storage>).Assembly.GetManifestResourceStream("parser.data." + path)
        use out = new System.IO.MemoryStream()
        input.CopyTo(out)
        let bytes = out.ToArray()
        let ints = Array.zeroCreate<int> (bytes.Length / 4)
        System.Buffer.BlockCopy(bytes, 0, ints, 0, bytes.Length)
        ints

    let userIcons = { names = readInts "user.names.dat"; icons = readInts "user.icons.dat" }
    let tagIcons = { names = readInts "tag.names.dat"; icons = readInts "tag.icons.dat" }

module Parsers =
    open HtmlAgilityPack
    open JoyReactor.Types
    open System
    type Regex = Text.RegularExpressions.Regex

    let private domain = UrlBuilder.domain

    let private fixProtocol (url: string) =
        url.Replace("http://", UrlBuilder.scheme)
        |> fun x -> Regex.Replace(x, "^//", "https://")

    let private getDocument html =
        let doc = HtmlDocument()
        doc.LoadHtml html
        doc.DocumentNode

    let csrfTokenName = "signin[_csrf_token]"

    let getCsrfToken html =
        let doc = getDocument html
        let tokenNode =
            sprintf "//input[@name='%s']" csrfTokenName
            |> doc.SelectSingleNode
        tokenNode.Attributes.["value"].Value

    let private resolveTagImage name =
        TagResolver.tryGetImageId TagResolver.tagIcons name
        |> Option.map ^ sprintf "https://img1.joyreactor.cc/pics/avatar/tag/%i"
        |> Option.defaultValue ^ sprintf "https://img0.%s/images/default_avatar.jpeg" domain

    let parseUserName html =
        let m = Regex.Match(html, "<a href=\"/user/([^\"]+)\"\\s+id=\"settings\"")
        if m.Success then Some m.Groups.[1].Value else None

    let parseTopTags html =
        let doc = getDocument html
        doc.QuerySelectorAll("#blogs_week_content img")
        |> Seq.map (fun x ->
            { name = HtmlEntity.DeEntitize x.Attributes.["alt"].Value;
              image = fixProtocol x.Attributes.["src"].Value })
        |> Seq.toArray

    let readUserTags html =
        let doc = getDocument html
        doc.QuerySelectorAll("h2.sideheader")
        |> Seq.filter (fun x -> x.InnerText = "Читает")
        |> Seq.collect (fun x -> x.NextSiblingElement().GetChildElements())
        |> Seq.map (fun x -> {
            name = Regex.Replace(x.InnerText, "[ \r\n\u00A0]+", " ") |> HtmlEntity.DeEntitize
            image = resolveTagImage (x.InnerText) })
        |> Seq.toArray

    let private normalizeUrl link =
        let replace regex (r : string) value = Regex.Replace(value, regex, r)
        link
        |> replace "(/comment/).+(-\\d+\\.[\\w\\d]+)$" "$1$2"
        |> replace "(/full/).+(-\\d+\\.)" "$1$2"
        |> replace "(/post/).+(-\\d+\\.)" "$1$2"
        |> fixProtocol

    let findNumber value =
        let NUMBER_REGEX = Regex("\\d+")
        int ^ NUMBER_REGEX.Match(value).Value

    let private tryMatch (r : Regex) value =
        let m = r.Match(value)
        if m.Success then Some m else None

    let private parserSinglePost (element : HtmlNode) =
        let getRating() =
            let RATING_REGEX = Regex("[\\d\\.]+")
            element.QuerySelector("span.post_rating > span")
            |> Option.ofObj
            |> Option.bind ^ fun e -> tryMatch RATING_REGEX e.InnerText
            |> Option.map ^ fun m -> float m.Value
            |> Option.defaultValue 0.0
        let getCreated() =
            let e = element.QuerySelector("span.date > span")
            DateTime(1970, 1, 1).AddSeconds(float e.Attributes.["data-time"].Value)
        let parseTagsInPost() =
            element.QuerySelectorAll(".taglist a")
            |> Seq.map ^ fun x -> x.InnerText
            |> Seq.toArray
        let queryImage _ =
            element.QuerySelectorAll("div.post_content img")
            |> Seq.filter ^ fun x -> x.Attributes.Contains("width")
            |> Seq.filter ^ fun x -> not (x.Attributes.["height"].Value.EndsWith("%"))
            |> Seq.map ^ fun x ->
                let aspect = (float x.Attributes.["width"].Value) / (float x.Attributes.["height"].Value)
                x.Attributes.["src"].Value
                |> normalizeUrl
                |> fun x -> { aspect = aspect; url = x }
            |> Seq.tryHead
        let parseAttachments() =
            let parserThumbnails (element : HtmlNode) =
                let getThumbnailImageLink (x : HtmlNode) : String =
                    let hasFull (img : HtmlNode) = "a" = img.ParentNode.Name
                    if hasFull x then
                        x.ParentNode.Attributes.["href"].Value
                        |> fun x -> Regex.Replace(x, "(/full/).+(-\\d+\\.)", "$1$2")
                    else
                        x.Attributes.["src"].Value
                        |> fun x -> Regex.Replace(x, "(/post/).+(-\\d+\\.)", "$1$2")
                    |> fixProtocol

                element.QuerySelectorAll("div.post_content img")
                |> Seq.filter ^ fun x -> x <> null && x.Attributes.Contains("width")
                |> Seq.filter ^ fun x -> not (x.Attributes.["height"].Value.EndsWith("%"))
                |> Seq.map ^ fun x ->
                    { aspect = (float x.Attributes.["width"].Value) / (float x.Attributes.["height"].Value)
                      url = getThumbnailImageLink (x) }

            let parseYoutubeThumbnails (element : HtmlNode) =
                let SRC_PATTERN = Regex("/embed/([^?]+)")
                element.QuerySelectorAll("iframe.youtube-player")
                |> Seq.map ^ fun x ->
                    let m = SRC_PATTERN.Match(x.Attributes.["src"].Value)
                    if not m.Success then failwith x.Attributes.["src"].Value
                    { aspect = (float x.Attributes.["width"].Value) / (float x.Attributes.["height"].Value)
                      url = sprintf "https://img.youtube.com/vi/%s/0.jpg" m.Groups.[1].Value }

            let parseVideoThumbnails (element : HtmlNode) =
                element.QuerySelectorAll("video[poster]")
                |> Seq.map ^ fun x ->
                    { aspect = (float x.Attributes.["width"].Value) / (float x.Attributes.["height"].Value)
                      url = element.QuerySelector("span.video_gif_holder > a").Attributes.["href"].Value
                            |> fun x -> Regex.Replace(x, "(/post/).+(-)", "$1$2") }

            element.QuerySelector("div.post_top")
                |> fun x -> Seq.concat [ parserThumbnails (x); parseYoutubeThumbnails (x); parseVideoThumbnails (x) ]
                |> Seq.map ^ fun x -> { image = x }
                |> Seq.toArray
        let parseComments() =
            let parseCommentAttachments (node : HtmlNode) =
                node.QuerySelectorAll("div.image img[width]")
                |> Seq.map ^ fun x ->
                    { image =
                        { aspect = (float x.Attributes.["width"].Value) / (float x.Attributes.["height"].Value)
                          url = normalizeUrl (x.Attributes.["src"].Value) } }
                |> Seq.toArray
            let parseFloat (value : string) =
                match Double.TryParse value with
                | true, x -> x
                | false, _ -> failwithf "Can't parse (%s) to float" value

            let postId = findNumber (element.Id)
            element.QuerySelectorAll("div.comment[parent]")
            |> Seq.map ^ fun node ->
                let parent = node.ParentNode
                let parentId =
                    if "comment_list" = parent.Attributes.["class"].Value
                        then findNumber (parent.Id)
                        else 0

                let userImg = node.QuerySelector("img.avatar")
                let id = int (node.QuerySelector("span.comment_rating").Attributes.["comment_id"].Value)
                { text = node.QuerySelector("div.txt > div").InnerText
                  image = { aspect = 1.0; url = fixProtocol userImg.Attributes.["src"].Value }
                  rating = node.QuerySelectorAll("span.comment_rating")
                           |> Seq.map ^ fun x -> x.InnerText
                           |> Seq.fold (sprintf "%s%s") ""
                           |> fun x -> x.Replace("≈", "").Trim()
                           |> parseFloat
                //   postId = postId
                //   id = (node.select("span.comment_rating").attr("comment_id")).toLong()
                  userName = userImg.Attributes.["alt"].Value
                  attachments = parseCommentAttachments (node)
                  id = id
                  parentId = parentId }
            |> Seq.toArray

        { userImage = {
            aspect = 1.0;
            url = fixProtocol (element.QuerySelector("div.uhead_nick > img").Attributes.["src"].Value) }
          userName = element.QuerySelector("div.uhead_nick > a").InnerText
          rating = getRating()
          created = getCreated()
          tags = parseTagsInPost()
          id = findNumber (element.Id)
          title = element.QuerySelectorAll("div.post_content > div > h3")
                  |> Seq.tryHead |> Option.map (fun x -> x.InnerText) |> Option.defaultValue ""
          image = queryImage (element) |> Option.toArray
          attachments = parseAttachments()
          comments = parseComments() }

    let parsePostsForTag html =
        let element = getDocument html
        element.QuerySelectorAll("div.postContainer")
        |> Seq.map parserSinglePost
        |> Seq.toArray

    let parseNewPageNumber html =
        let extractPageFromHref (x : HtmlNode) =
            x.Attributes.["href"].Value.Split('/')
            |> Seq.last
            |> findNumber

        let element = getDocument html
        element.QuerySelectorAll("a.next")
        |> Seq.tryPick ^ fun x -> Some ^ extractPageFromHref x

    let parsePostsWithNext html =
        { posts = parsePostsForTag html
          nextPage = parseNewPageNumber html |> Option.toArray }

    let parsePost html =
        let element = getDocument html
        element.QuerySelector("div.postContainer")
        |> Option.ofObj
        |> Option.map parserSinglePost

    let getMessages html =
        let getUserImage (name : String) : String =
            TagResolver.tryGetImageId TagResolver.userIcons name
            |> Option.map ^ sprintf "https://img0.%s/pics/avatar/user/%i" domain
            |> Option.defaultValue ^ sprintf "https://img0.%s/images/default_avatar.jpeg" domain

        let document = getDocument html
        let messages =
            document.QuerySelectorAll("div.messages_wr > div.article")
            |> Seq.map ^ fun x ->
                let username = x.QuerySelector("div.mess_from > a").InnerText
                { text = x.QuerySelector("div.mess_text").InnerText.Trim()
                  date = 1000.0 * (double ^ x.QuerySelector("span[data-time]").Attributes.["data-time"].Value)
                  isMine = Seq.isEmpty ^ x.QuerySelectorAll("div.mess_reply")
                  userName = username
                  userImage = getUserImage (username) }
            |> Seq.toArray

        let nextPage =
            document.QuerySelectorAll("a.next")
            |> Seq.tryPick ^ fun x -> Some x.Attributes.["href"].Value

        if Seq.isEmpty messages
            then None
            else Some { messages = messages; nextPage = nextPage }

    let profile html =
        try
            let document = getDocument html

            let getProgressToNewStar =
                let style = document.QuerySelector("div.stars div.poll_res_bg_active").Attributes.["style"].Value
                let m = Regex("width:(\\d+)%;").Match(style)
                if not m.Success then failwith ""
                float ^ m.Groups.[1].Value

            { userName = document.QuerySelector("div.sidebarContent > div.user > span").InnerText.Trim()
              userImage =
                { aspect = 1.0;
                  url = fixProtocol (document.QuerySelector("div.sidebarContent > div.user > img").Attributes.["src"].Value) }
              rating = float ^ document.QuerySelector("#rating-text > b").InnerText.Replace(" ", "")
              stars = Seq.length ^ document.QuerySelectorAll(".star-row-0 > .star-0")
              progressToNewStar = getProgressToNewStar }
            |> Some
        with
        | _ -> None
