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
    open JoyReactor
    open JoyReactor.Types

    let private wrapToJsonOption isEmpty (xs : _) : _ JsonOption =
        match isEmpty xs with
        | true -> [||]
        | false -> [| xs |]

    let parse url ctx =
        async {
            let! html = Downloader.loadHtml url
            let wp =
                { posts = Parsers.parsePostsWithNext html |> wrapToJsonOption (fun x -> Array.isEmpty x.posts)
                  userName = Parsers.parseUserName html |> JsonOption.fromOption
                  userTags = Parsers.readUserTags html |> wrapToJsonOption Array.isEmpty
                  topTags = Parsers.parseTopTags html |> wrapToJsonOption Array.isEmpty }
                |> JsonSerializer.SerializeToUtf8Bytes
                |> Successful.ok
            return! wp ctx
        }

    let posts url ctx =
        async {
            let! html = Downloader.loadHtml url
            let wp = 
                JoyReactor.Parsers.parsePostsWithNext html
                |> JsonSerializer.SerializeToUtf8Bytes
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
