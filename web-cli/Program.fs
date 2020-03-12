let (@@) f x = f x

module Foo =
    open Suave

    let loadHtml url : string Async = failwith "???"

    let feed = 
        request @@ fun _ ctx ->
            async {
                let! html = loadHtml "http://joyreactor.cc/"
                let posts = JoyReactor.Parsers.parsePostsForTag html
                let json = Json.toJson posts
                return! Successful.ok json ctx
            }

open Suave
open Suave.Operators
open Suave.Filters

[<EntryPoint>]
let main argv =
    choose [
        GET >=> path "/info" >=> Successful.OK(sprintf "JR Parser (Suave) - %O" System.DateTime.Now)
        GET >=> path "/feed" >=> Foo.feed ]
    |> startWebServer {
        defaultConfig with
            bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8080us ] }
    0
