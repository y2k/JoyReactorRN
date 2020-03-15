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
        GET >=> pathScan "/posts/%s" Domain.posts >=> (Writers.setHeader  "Access-Control-Allow-Origin" "*")
        GET >=> path "/info" >=> Successful.OK(sprintf "JR Parser (Suave) - %O" System.DateTime.Now) ]
    |> startWebServer {
        defaultConfig with
            bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8090us ] }
    0
