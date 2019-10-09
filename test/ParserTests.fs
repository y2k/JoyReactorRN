module ParserTests

open JoyReactor
open JoyReactor.Types
open Xunit
type R = System.Text.RegularExpressions.Regex

let getHtml name =
    sprintf "%s/Resources/%s" __SOURCE_DIRECTORY__ name
    |> System.IO.File.ReadAllText

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
    let actual = getHtml "tags_test.html" |> Parsers.readTags
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
    Assert.Equal(1, List.length actual)
    Assert.Equal(None, Parsers.parseNewPageNumber element)

[<Fact>]
let ``parse feed with top comment``() =
    Parsers.parsePostsForTag (getHtml "feed_with_top_comment.html")

[<Fact>]
let ``parse posts with 5 comments``() =
    let post = Parsers.parsePost (getHtml "post_with_5_comments.html")

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
    let actual = Parsers.profile (getHtml "profile.html")
    Assert.Equal(
        { userName = "_y2k"
          userImage = { aspect = 1.0; url = "http://img1.joyreactor.cc/pics/avatar/user/331291" }
          rating = 40.0
          stars = 1
          progressToNewStar = 67.0 },
        actual)

[<Fact>]
let ``parse posts with image in comments``() =
        let post = Parsers.parsePost (getHtml "post_with_image_in_comments.html")

        Assert.Equal(
            [ []; []; []; []; []; []; [ { aspect = 1.0; url = "http://img1.joyreactor.cc/pics/comment/-3039289.jpeg" } ]; [] ]
            |> box,
            post.comments
            |> Seq.map ^ fun x -> x.attachments |> Seq.map ^ fun x -> x.image
            |> box)
