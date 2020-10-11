module JoyReactor.Tests.UseCasesTests

open Xunit
open Swensen.Unquote
open JoyReactor.Components

module TestFramework =
    open System
    open Elmish
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
        options.WriteIndented <- true

        let viewTo viewRef (model : ApplicationScreen.Model) =
            let model =
                match model.history.[0] with
                | ApplicationScreen.PostModel x -> box x
                | ApplicationScreen.PostsModel x -> box x
                | ApplicationScreen.TabsModel x -> box x
                | ApplicationScreen.MessagesModel x -> box x
            let view = JsonSerializer.Serialize (model, options)
            match !viewRef with
            | [] -> viewRef := [ view ]
            | prev :: _ when prev <> view -> viewRef := view :: !viewRef
            | _ -> ()

    let assertTest f =
        JoyReactor.ActionModule.resetDb ()
        JoyReactor.ActionModule.downloadAndParseImpl <- SyncDownloader.downloadImpl
        let viewRef : string list ref = ref []
        let dispatch : (ApplicationScreen.Msg -> unit) ref = ref (fun _ -> ())
        Program.mkProgram D.init (flip D.update) (fun model d -> dispatch := d; TestRenderer.viewTo viewRef model)
        |> Program.run
        f (List.rev !viewRef) (fun x -> viewRef := []; !dispatch (ApplicationScreen.TabsMsg x); List.rev !viewRef)

    let getJson id =
        List.unfold
            (fun index ->
                let path = sprintf "../../../Resources/json/%s.%i.json" id index
                if IO.File.Exists path then Some (IO.File.ReadAllText path, index + 1)
                else None)
            0

    [<Obsolete("For testing use")>]
    let saveJson id jsons =
        jsons
        |> List.iteri
            (fun index json ->
                let path = sprintf "../../../Resources/json/%s.%i.json" id index
                IO.File.WriteAllText (path, json))

module T = TestFramework

[<Fact>]
let ``open post test`` () =
    T.assertTest <| fun _ dispatch ->
        let actual = FeedScreen.OpenPost 4444270 |> TabsScreen.FeedMsg |> dispatch
        let expected = T.getJson "9a7e041f"
        test <@ actual = expected @>

[<Fact>]
let ``open tags test`` () =
    T.assertTest <| fun _ dispatch ->
        let actual = TabsScreen.SelectPage 1 |> dispatch
        let expected = T.getJson "aed3391d"
        test <@ expected = actual @>

        let actual = TagsScreen.OpenTag "purpleisaprose" |> TabsScreen.TagsMsg  |> dispatch
        let expected = T.getJson "db255b1d"
        test <@ expected = actual @>

[<Fact>]
let ``feed test`` () =
    T.assertTest <| fun initView dispatch ->
        let expected = T.getJson "60c93f24"
        test <@ expected = initView @>

        let actual = FeedScreen.LoadNextPage |> TabsScreen.FeedMsg |> dispatch
        let expected = T.getJson "6e1ee9f0"
        test <@ expected = actual @>
