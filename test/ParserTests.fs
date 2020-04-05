module ParserTests

type R = System.Text.RegularExpressions.Regex
type F = System.IO.File
type D = System.IO.Directory

open System
open JoyReactor
open JoyReactor.Types
open Xunit
open Swensen.Unquote

let RESOURCES_DIR = sprintf "%s/Resources" __SOURCE_DIRECTORY__

let getHtml name =
    sprintf "%s/%s" RESOURCES_DIR name
    |> F.ReadAllText

let private assertMessages (actual : MessagesWithNext) =
    actual.messages
    |> Array.iter ^ fun message ->
        let toUnixTime year = (DateTime(year, 1, 1) - DateTime(1970, 1, 1)).TotalMilliseconds
        Assert.InRange(message.date, toUnixTime 2015, toUnixTime 2017)
        Assert.Matches(R(@"^http://img\w\.joyreactor\.cc/pics/avatar/user/\w{6}$"), message.userImage)
        Assert.Matches(R(@"^[\w\d_]{4,6}$"), message.userName)
        Assert.Equal(message.text.Trim([| '\n'; '\r'; '\t'; ' ' |]), message.text)
        
[<Fact>]
let ``get csrf token``() =
    let token = getHtml "login.html" |> Parsers.getCsrfToken
    test <@ "fc30296a9b71af8738ec6d3fd5754203" = token @>
    ()

[<Fact>]
let ``parse first messages``() =
    Assert.Equal(None, getHtml "feed.html" |> Parsers.getMessages)

    let actual = getHtml "messages_first.html" |> Parsers.getMessages |> Option.get
    Assert.Equal(Some "/private/list/2", actual.nextPage)
    Assert.Equal(20, Seq.length actual.messages)
    Assert.Equal(10, actual.messages |> Seq.filter (fun x -> x.isMine) |> Seq.length)
    Assert.Equal(10, actual.messages |> Seq.filter (fun x -> not x.isMine) |> Seq.length)
    assertMessages actual
        
[<Fact>]
let ``parse lst messages``() =
    let actual = getHtml "messages_last.html" |> Parsers.getMessages |> Option.get
    Assert.Equal(None, actual.nextPage)
    Assert.Equal(3, Seq.length actual.messages)
    Assert.Equal(0, actual.messages |> Seq.filter (fun x -> x.isMine) |> Seq.length)
    Assert.Equal(3, actual.messages |> Seq.filter (fun x -> not x.isMine) |> Seq.length)
    assertMessages actual

[<Fact(Skip = "FIXME")>]
let ``parsers should not crash``() =
    RESOURCES_DIR
    |> D.GetFiles
    |> Array.iter ^ fun file ->
        let html = F.ReadAllText file
        try
            Parsers.profile html |> ignore
            Parsers.getMessages html |> ignore
            Parsers.parsePost html |> ignore
            Parsers.parseTopTags html |> ignore
            Parsers.parseUserName html |> ignore
            Parsers.parsePostsForTag html |> ignore
            Parsers.parsePostsWithNext html |> ignore
            Parsers.parseNewPageNumber html |> ignore
        with
        | e -> raise ^ Exception (file, e)

[<Fact>]
let ``read empty post``() =
    let actual = getHtml "messages_first.html" |> Parsers.parsePost
    Assert.Equal(None, actual)

[<Fact>]
let ``get username``() =
    let actual = getHtml "messages_first.html" |> Parsers.parseUserName
    Assert.Equal(Some "user500", actual)

[<Fact>]
let ``parse post``() =
    getHtml "post_4111388.html" |> Parsers.parsePost

[<Fact>]
let ``reading top tags``() =
    let actual = getHtml "feed_with_top_comment.html" |> Parsers.parseTopTags
    Assert.Equal(10, Seq.length actual)
    actual
    |> Seq.iter ^
        fun tag ->
            Assert.True(tag.name = tag.name.Trim(), tag.name)
            Assert.Matches(R(@"^[\w &]{1,30}$"), tag.name)
            Assert.Matches(R(@"^http://img\d\.joyreactor\.cc/pics/avatar/tag/\d+$"), tag.image)

[<Fact>]
let ``reading tags``() =
    let actual = getHtml "tags_test.html" |> Parsers.readUserTags
    Assert.Equal(36, Seq.length actual)
    actual
    |> Seq.iter ^
        fun tag ->
            Assert.True(tag.name = tag.name.Trim(), tag.name)
            Assert.Matches(R(@"^[а-я\w \-&]{4,30}$"), tag.name)
            Assert.Matches(R(@"^http://img\d\.joyreactor\.cc/(images/default_avatar.jpeg|pics/avatar/tag/\d+)$"), tag.image)

[<Fact>]
let ``parse small_favorite should success``() =
    let element = getHtml "small_favorite.html"

    let actual = Parsers.parsePostsForTag (element)
    Assert.Equal(1, Seq.length actual)
    Assert.Equal(None, Parsers.parseNewPageNumber element)

[<Fact>]
let ``parse feed``() =
    let actual = Parsers.parsePostsForTag (getHtml "feed.html")
    let images = actual |> Seq.choose ^ fun post -> post.image |> Seq.tryHead |> Option.map (fun x -> x.url)
    Assert.Equal(9, Seq.length images)
    images 
    |> Seq.iter ^ fun image ->
        Assert.Matches(R(@"http://img\d\.joyreactor\.cc/pics/post/-\d+\.(gif|jpeg|png)"), image)

[<Fact>]
let ``parse feed with top comment``() =
    let actual = Parsers.parsePostsForTag (getHtml "feed_with_top_comment.html")
    actual
    |> Seq.iter ^ fun post ->
        Assert.Matches(
            R(@"http://img\d\.joyreactor\.cc/pics/post/-\d+\.(gif|jpeg|png)"), 
            post.image |> Array.tryHead |> Option.map (fun x -> x.url) |> Option.defaultValue "")

[<Fact>]
let ``parse posts with 5 comments``() =
    let post = Parsers.parsePost (getHtml "post_with_5_comments.html") |> Option.get

    Assert.Equal (
        box ([ "Pinguin"; "WRZESZCZ"; "Diablero"; "LYVrus"; "Cobold" ]),
        box (post.comments |> Seq.map (fun it -> it.userName)))

    Assert.Equal(
        [ []; []; []; [ { aspect = 290.0 / 245.0; url = "http://img0.joyreactor.cc/pics/comment/-2583972.gif" } ]; [] ]
        |> box,
        post.comments
        |> Seq.map (fun it -> it.attachments |> Seq.map (fun it -> it.image))
        |> box)

    Assert.Equal(5, post.comments.Length)

[<Fact>]
let ``parse profile should success``() =
    let actual = Parsers.profile (getHtml "profile.html") |> Option.get
    Assert.Equal(
        { userName = "_y2k"
          userImage = { aspect = 1.0; url = "http://img1.joyreactor.cc/pics/avatar/user/331291" }
          rating = 40.0
          stars = 1
          progressToNewStar = 67.0 },
        actual)

[<Fact>]
let ``parse posts with image in comments``() =
        let post = Parsers.parsePost (getHtml "post_with_image_in_comments.html") |> Option.get

        Assert.Equal(
            [ []; []; []; []; []; []; [ { aspect = 1.0; url = "http://img1.joyreactor.cc/pics/comment/-3039289.jpeg" } ]; [] ]
            |> box,
            post.comments
            |> Seq.map ^ fun x -> x.attachments |> Seq.map ^ fun x -> x.image
            |> box)
