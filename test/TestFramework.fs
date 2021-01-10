module JoyReactor.Tests.TestFramework

open System
open Xunit
open Elmish
open JoyReactor.Components

module D = ApplicationScreen
type R = Text.RegularExpressions.Regex

module SyncDownloader =
    open JoyReactor.WebCli

    let private downloadHtml (url: string) =
        let encodedUrl =
            Text.Encoding.UTF8.GetBytes url
            |> Convert.ToBase64String

        let path =
            sprintf "../../../Resources/htmls/%s.html" encodedUrl

        if not <| IO.File.Exists path then
            printfn "Can't find url %s (%s)" url path

        IO.File.ReadAllText path |> async.Return

    let downloadImpl url =
        Domain.parseInner
            (fun (url: string) ->
                async {
                    let! body = downloadHtml url
                    return body, []
                })
            url
        |> Async.map fst

module TestRenderer =
    open System.Text.Json

    let private options = JsonSerializerOptions()
    options.Converters.Add(Serialization.JsonFSharpConverter())
    options.Encoder <- Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    options.WriteIndented <- true

    let viewTo viewRef (model: ApplicationScreen.Model) =
        let model =
            match model.history.[0] with
            | ApplicationScreen.PostModel x -> box x
            | ApplicationScreen.PostsModel x -> box x
            | ApplicationScreen.TabsModel x -> box x
            | ApplicationScreen.MessagesModel x -> box x

        let view = JsonSerializer.Serialize(model, options)

        match !viewRef with
        | [] -> viewRef := [ view ]
        | prev :: _ when prev <> view -> viewRef := view :: !viewRef
        | _ -> ()

type Env =
    { dispatch: TabsScreen.Msg -> string list
      initView: string list
      getJson: string -> string list
      assetJson: string -> int -> string list -> unit }

let private getJson id =
    List.unfold
        (fun index ->
            let path =
                sprintf "../../../Resources/json/%s.%i.json" id index

            if IO.File.Exists path then
                Some(IO.File.ReadAllText path, index + 1)
            else
                None)
        0

let private saveJson id jsons =
    jsons
    |> List.iteri
        (fun index json ->
            let path =
                sprintf "../../../Resources/json/%s.%i.json" id index

            IO.File.WriteAllText(path, json))

open Swensen.Unquote

let assetJson id count actual =
    match getJson id with
    | [] ->
        test <@ count = List.length actual @>
        saveJson id actual
    | expected -> test <@ actual = expected @>

let assertTest f =
    JoyReactor.ActionModule.resetDb ()
    JoyReactor.ActionModule.downloadAndParseImpl <- SyncDownloader.downloadImpl

    let viewRef: string list ref = ref []
    let dispatch: (ApplicationScreen.Msg -> unit) ref = ref (fun _ -> ())

    Program.mkProgram
        D.init
        (flip D.update)
        (fun model d ->
            dispatch := d
            TestRenderer.viewTo viewRef model)
    |> Program.run

    f
        { dispatch =
              (fun x ->
                  viewRef := []
                  ! dispatch (ApplicationScreen.TabsMsg x)
                  List.rev !viewRef)
          initView = (List.rev !viewRef)
          getJson = getJson
          assetJson = assetJson }
