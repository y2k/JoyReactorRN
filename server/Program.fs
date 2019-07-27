open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

module Routers =
    open JoyReactor

    let parsePost = 
        request ^ fun r -> 
            r.formData "html" 
            |> Choice.fold id (fun _ -> failwith "???")
            |> Parsers.parsePost
            |> Json.toJson
            |> ok

[<EntryPoint>]
let main _ =
    choose [
        GET >=> path "/info" >=> OK (sprintf "JR Parser (Suave) - %O" System.DateTime.Now)
        POST >=> path "/post" >=> Routers.parsePost
    ]
    |> startWebServer { defaultConfig with bindings = [ HttpBinding.createSimple Protocol.HTTP "0.0.0.0" 8080 ] }
    0
