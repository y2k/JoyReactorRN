module Routers =
    open Suave
    open Suave.Successful
    open Newtonsoft.Json
    open JoyReactor

    let callParser parser =
        request ^ fun r ->
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

[<EntryPoint>]
let main _ =
    choose [
        GET >=> path "/info" >=> OK(sprintf "JR Parser (Suave) - %O" System.DateTime.Now)
        POST >=> path "/messages" >=> Routers.callParser P.getMessages
        POST >=> path "/profile" >=> Routers.callParser P.profile
        POST >=> path "/tags" >=> Routers.callParser P.readTags
        POST >=> path "/post" >=> Routers.callParser P.parsePost
        POST >=> path "/posts" >=> Routers.callParser (fun html -> { posts = P.parsePostsForTag html; nextPage = None })
    ]
    |> startWebServer { defaultConfig with bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8080us ] }
    0
