let (@@) f x = f x

module Foo =
    module Downloader =
        open System.Net.Http
        let loadHtml (url : string) : string Async = 
            async {
                use httpClient = new HttpClient()
                return! httpClient.GetStringAsync url |> Async.AwaitTask
            }

    open Suave
    open System.Text.Json

    let feed = 
        request @@ fun _ ctx ->
            async {
                let! html = Downloader.loadHtml "http://joyreactor.cc/"
                let posts = JoyReactor.Parsers.parsePostsForTag html
                let json = JsonSerializer.SerializeToUtf8Bytes posts
                return! Successful.ok json ctx
            }

open Suave
open Suave.Operators
open Suave.Filters

[<EntryPoint>]
let main _ =
    choose [
        GET >=> path "/info" >=> Successful.OK(sprintf "JR Parser (Suave) - %O" System.DateTime.Now)
        GET >=> path "/feed" >=> Foo.feed ]
    |> startWebServer {
        defaultConfig with
            bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8090us ] }
    0
