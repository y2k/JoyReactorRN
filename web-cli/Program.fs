module Domain =
    module Downloader =
        open System.Net.Http
        let loadHtml (url : string) : string Async = 
            async {
                use httpClient = new HttpClient()
                return! httpClient.GetStringAsync url |> Async.AwaitTask
            }

    open Suave
    open System.Text.Json
    open System.Text.Json.Serialization
    open JoyReactor
    open JoyReactor.Types

    let private options = JsonSerializerOptions()
    options.Converters.Add(JsonFSharpConverter())

    let private wrapToOption isEmpty xs =
        match isEmpty xs with
        | true -> None
        | false -> Some xs

    let parse url ctx =
        async {
            let! html = Downloader.loadHtml url
            let wp =
                { posts = Parsers.parsePostsWithNext html |> wrapToOption (fun x -> Array.isEmpty x.posts)
                  userName = Parsers.parseUserName html
                  userTags = Parsers.readUserTags html |> wrapToOption Array.isEmpty
                  topTags = Parsers.parseTopTags html |> wrapToOption Array.isEmpty
                  post = Parsers.parsePost html }
                |> fun response -> JsonSerializer.SerializeToUtf8Bytes (response, options)
                |> Successful.ok
            return! wp ctx
        }

open Suave
open Suave.Operators
open Suave.Filters

[<EntryPoint>]
let main _ =
    choose [
        GET >=> pathScan "/parse/%s" Domain.parse >=> (Writers.setHeader  "Access-Control-Allow-Origin" "*")
        GET >=> path "/info" >=> Successful.OK(sprintf "JR Parser (Suave) - %O" System.DateTime.Now) ]
    |> startWebServer {
        defaultConfig with
            bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8090us ] }
    0
