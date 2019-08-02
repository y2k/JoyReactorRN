module Routers =
    open Suave
    open Suave.Successful
    open Newtonsoft.Json
    open JoyReactor
    open JoyReactor.Types

    let parsePost = 
        request ^ fun r -> 
            r.formData "html" 
            |> Choice.fold id (fun _ -> failwith "???")
            |> Parsers.parsePost
            |> Json.toJson
            |> ok

    let parsePosts =
        request ^ fun r -> 
            let html =
                r.rawForm 
                |> System.Text.Encoding.UTF8.GetString

            let result = 
                { posts = Parsers.parsePostsForTag html
                  nextPage = None }

            result
            |> JsonConvert.SerializeObject
            |> OK

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

[<EntryPoint>]
let main _ =
    choose [
        GET >=> path "/info" >=> OK (sprintf "JR Parser (Suave) - %O" System.DateTime.Now)
        POST >=> path "/post" >=> Routers.parsePost
        POST >=> path "/posts" >=> Routers.parsePosts
    ]
    |> startWebServer { defaultConfig with bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8080us ] }
    0
