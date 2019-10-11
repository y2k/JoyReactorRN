module Routers =
    open Suave
    open Suave.Successful
    open Newtonsoft.Json

    let callParser parser =
        request ^ fun r ->
            printfn "request = %O" r
            r.rawForm
            |> System.Text.Encoding.UTF8.GetString
            |> parser
            |> JsonConvert.SerializeObject
            |> OK

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open JoyReactor.Types
module P = JoyReactor.Parsers

type CustomLogger() =
    interface Suave.Logging.Logger with
        member __.logWithAck _ _ = async.Zero ()
        member __.name = [| "" |]
        member __.log level line = printfn "LOGS === %O | %O" level ^ line level

[<EntryPoint>]
let main _ =
    choose [
        GET >=> path "/info" >=> OK(sprintf "JR Parser (Suave) - %O" System.DateTime.Now)
        POST >=> path "/messages" >=> Routers.callParser P.getMessages
        POST >=> path "/profile" >=> Routers.callParser P.profile
        POST >=> path "/toptags" >=> Routers.callParser P.parseTopTags
        POST >=> path "/tags" >=> Routers.callParser P.readTags
        POST >=> path "/post" >=> Routers.callParser P.parsePost
        POST >=> path "/posts" >=> Routers.callParser (fun html -> { posts = P.parsePostsForTag html; nextPage = None })
    ]
    |> startWebServer {
        defaultConfig with
            logger = new CustomLogger()
            bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8080us ]
    }
    0
