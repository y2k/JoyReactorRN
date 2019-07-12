open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

[<EntryPoint>]
let main _ =
    startWebServer defaultConfig (GET >=> path "/info" >=> OK (sprintf "JR Parser (Suave) - %O" System.DateTime.Now))
    0
