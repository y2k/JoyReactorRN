module JoyReactor.Tests.UseCasesTests

open System
open Elmish
open Swensen.Unquote
open Xunit
open JoyReactor.Components

module D = ApplicationScreen
type R = Text.RegularExpressions.Regex

module SyncDownloader = 
    open JoyReactor.WebCli

    let private downloadHtml (url : string) = 
        let encodedUrl = Text.Encoding.UTF8.GetBytes url |> Convert.ToBase64String
        let path = sprintf "../../../Resources/htmls/%s.html" encodedUrl
        if not <| IO.File.Exists path then printfn "Can't find url %s (%s)" url path
        IO.File.ReadAllText path |> async.Return
    let downloadImpl url =
        Domain.parseInner 
            (fun (url : string) -> 
                async {
                    let! body = downloadHtml url
                    return body, []
                }) url
        |> Async.map fst

module TestRenderer =
    open System.Text.Json
    let private options = JsonSerializerOptions()
    options.Converters.Add(Serialization.JsonFSharpConverter())
    options.Encoder <- Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    let view model = JsonSerializer.Serialize (model, options)

[<Fact>]
let ``first open test`` () =
    JoyReactor.ActionModule.downloadAndParseImpl <- SyncDownloader.downloadImpl
    async {
        let viewRef : string list ref = ref []
        let dispatch : (ApplicationScreen.Msg -> unit) ref = ref (fun _ -> ())
        Program.mkProgram 
            D.init 
            (flip D.update) 
            (fun model d -> 
                dispatch := d
                viewRef := TestRenderer.view model :: !viewRef)
        |> Program.run

        let actual = !viewRef
        viewRef := []

        let expected = 
            [ """{"history":[{"Case":"TabsModel","Fields":[{"Case":"FeedModel","Fields":[{"source":{"Case":"FeedSource"},"items":[{"Case":"Actual","Fields":[{"id":4443397,"userName":"Mobiligidus","userImage":{"url":"http://img0.joyreactor.cc/pics/avatar/user/164004","aspect":1},"rating":13.5,"created":"2020-07-27T13:53:31","image":[{"url":"http://img0.joyreactor.cc/pics/post/-6071768.jpeg","aspect":0.7502312673450509}],"attachments":[{"image":{"url":"http://img0.joyreactor.cc/pics/post/full/-6071768.jpeg","aspect":0.7502312673450509}}],"title":"","tags":["котэ","доминируй властвуй унижай","хоба"],"comments":[]}]},{"Case":"Actual","Fields":[{"id":4443512,"userName":"Olmer-85","userImage":{"url":"http://img1.joyreactor.cc/pics/avatar/user/597303","aspect":1},"rating":13.7,"created":"2020-07-27T15:35:09","image":[{"url":"http://img0.joyreactor.cc/pics/post/-6072046.jpeg","aspect":0.983030303030303}],"attachments":[{"image":{"url":"http://img0.joyreactor.cc/pics/post/full/-6072046.jpeg","aspect":0.983030303030303}}],"title":"Artist","tags":["heikala","artist","art","красивые картинки"],"comments":[]}]},{"Case":"Actual","Fields":[{"id":4443475,"userName":"AliaDen","userImage":{"url":"http://img1.joyreactor.cc/pics/avatar/user/548197","aspect":1},"rating":30.7,"created":"2020-07-27T14:47:01","image":[{"url":"http://img10.joyreactor.cc/pics/post/-6071994.jpeg","aspect":1.4128919860627178}],"attachments":[{"image":{"url":"http://img10.joyreactor.cc/pics/post/full/-6071994.jpeg","aspect":1.4128919860627178}}],"title":"","tags":["tokumaro","Anime Art","artist","art девушка","art","пицца","еда"],"comments":[]}]},{"Case":"Actual","Fields":[{"id":4443541,"userName":"kreuz","userImage":{"url":"http://img1.joyreactor.cc/pics/avatar/user/51927","aspect":1},"rating":25.8,"created":"2020-07-27T16:04:41","image":[],"attachments":[{"image":{"url":"http://img0.joyreactor.cc/pics/post/-6072116.gif","aspect":0.821917808219178}}],"title":"","tags":["гифки","собаки","встреча"],"comments":[]}]},{"Case":"Actual","Fields":[{"id":4443485,"userName":"commissarmanul","userImage":{"url":"http://img0.joyreactor.cc/pics/avatar/user/249414","aspect":1},"rating":14.3,"created":"2020-07-27T15:04:08","image":[{"url":"http://img0.joyreactor.cc/pics/post/-6072010.png","aspect":1.0464516129032257}],"attachments":[{"image":{"url":"http://img0.joyreactor.cc/pics/post/full/-6072010.png","aspect":1.0464516129032257}},{"image":{"url":"http://img1.joyreactor.cc/pics/post/full/-6072011.png","aspect":0.7600749765698219}}],"title":"","tags":["BB&#039;s drawings","artist","#Anime","фэндомы"],"comments":[]}]},{"Case":"Actual","Fields":[{"id":4443483,"userName":"LittleSotoninka","userImage":{"url":"http://img0.joyreactor.cc/pics/avatar/user/196574","aspect":1},"rating":20.9,"created":"2020-07-27T15:03:24","image":[{"url":"http://img0.joyreactor.cc/pics/post/-6072008.png","aspect":0.7082969432314411}],"attachments":[{"image":{"url":"http://img0.joyreactor.cc/pics/post/full/-6072008.png","aspect":0.7082969432314411}}],"title":"Постер-кроссовер","tags":["Yomu (Sgt Epper)","Anime Art","artist","Himura Kiseki","kouhai-chan (tawawa)"],"comments":[]}]},{"Case":"Actual","Fields":[{"id":4443346,"userName":"Красный Комиссар","userImage":{"url":"http://img1.joyreactor.cc/pics/avatar/user/894697","aspect":1},"rating":29.2,"created":"2020-07-27T13:11:50","image":[{"url":"http://img0.joyreactor.cc/pics/post/-6071708.jpeg","aspect":1}],"attachments":[{"image":{"url":"http://img0.joyreactor.cc/pics/post/-6071708.jpeg","aspect":1}}],"title":"","tags":["рамзан кадыров","политика","обезьяна с гранатой"],"comments":[]}]},{"Case":"Actual","Fields":[{"id":4443384,"userName":"Nox7662","userImage":{"url":"http://img1.joyreactor.cc/pics/avatar/user/425193","aspect":1},"rating":23.5,"created":"2020-07-27T13:48:07","image":[{"url":"http://img1.joyreactor.cc/pics/post/-6071749.jpeg","aspect":0.7221727515583259}],"attachments":[{"image":{"url":"http://img1.joyreactor.cc/pics/post/full/-6071749.jpeg","aspect":0.7221727515583259}}],"title":"Celtic Warrior","tags":["Воины (Fantasy)","Fantasy","art","Jian Li","кельты"],"comments":[]}]},{"Case":"Actual","Fields":[{"id":4443135,"userName":"commissarmanul","userImage":{"url":"http://img0.joyreactor.cc/pics/avatar/user/249414","aspect":1},"rating":38.1,"created":"2020-07-27T10:01:04","image":[{"url":"http://img10.joyreactor.cc/pics/post/-6071456.jpeg","aspect":0.7076788830715532}],"attachments":[{"image":{"url":"http://img10.joyreactor.cc/pics/post/full/-6071456.jpeg","aspect":0.7076788830715532}}],"title":"","tags":["noart 0","Yunyun (KonoSuba)","KonoSuba","Anime","Cyberpunk 2077","Игры","crossover"],"comments":[]}]},{"Case":"Actual","Fields":[{"id":4442478,"userName":"uni-snake","userImage":{"url":"http://img0.joyreactor.cc/pics/avatar/user/240390","aspect":1},"rating":13.5,"created":"2020-07-26T18:55:50","image":[{"url":"http://img1.joyreactor.cc/pics/post/-6070425.jpeg","aspect":0.8085742771684945}],"attachments":[{"image":{"url":"http://img1.joyreactor.cc/pics/post/full/-6070425.jpeg","aspect":0.8085742771684945}}],"title":"","tags":["Мрачные картинки","art","самурай","Mert Genccinar","artist"],"comments":[]}]},{"Case":"LoadNextDivider"}],"hasNew":false,"loading":false}]}]}]}""";
              """{"history":[{"Case":"TabsModel","Fields":[{"Case":"FeedModel","Fields":[{"source":{"Case":"FeedSource"},"items":[],"hasNew":false,"loading":false}]}]}]}""";
              """{"history":[{"Case":"TabsModel","Fields":[{"Case":"FeedModel","Fields":[{"source":{"Case":"FeedSource"},"items":[],"hasNew":true,"loading":false}]}]}]}""";
              """{"history":[{"Case":"TabsModel","Fields":[{"Case":"FeedModel","Fields":[{"source":{"Case":"FeedSource"},"items":[],"hasNew":false,"loading":true}]}]}]}""";
              """{"history":[{"Case":"TabsModel","Fields":[{"Case":"FeedModel","Fields":[{"source":{"Case":"FeedSource"},"items":[],"hasNew":false,"loading":false}]}]}]}""" ] 
        test <@ expected = actual @>

        FeedScreen.LoadNextPage
        |> TabsScreen.FeedMsg
        |> ApplicationScreen.TabsMsg
        |> !dispatch

        let actual = !viewRef
        viewRef := []
        test <@ [] = actual @>
    } |> Async.RunSynchronously
