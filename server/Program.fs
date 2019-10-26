module Server

module Routers =
    open Suave
    open Suave.Successful
    type D = JoyReactor.SyncStore.Diff

    let sync =
        request ^ fun r ->
            r.multiPartFields
            |> ParseDomain.sync
            |> fun (D.Diff bytes) -> ok bytes

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

type CustomLogger() =
    interface Logging.Logger with
        member __.logWithAck _ _ = async.Zero ()
        member __.name = [| "" |]
        member __.log level line = printfn "LOGX :: %O | %O" level ^ line level

[<EntryPoint>]
let main _ =
    JoyReactor.SyncStore.toJsonString := Newtonsoft.Json.JsonConvert.SerializeObject

    choose [
        GET >=> path "/info" >=> OK(sprintf "JR Parser (Suave) - %O" System.DateTime.Now)
        POST >=> path "/sync" >=> Routers.sync ]
    |> startWebServer {
        defaultConfig with
            bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8080us ] }
    0
