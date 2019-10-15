module ParseDomain =
    module S = JoyReactor.SyncStore
    module P = JoyReactor.Parsers

    let parse html (db : JoyReactor.CofxStorage.LocalDb) =
        let p = P.parsePost html
        let tags = P.parseTopTags html
        { db with
             topTags = 
                if (Seq.isEmpty tags) 
                    then db.topTags 
                    else tags |> Array.toSeq |> Seq.map (fun x -> x.name, x) |> Map.ofSeq
             posts = Map.add p.id p db.posts }

    let sync = 
        S.listenUpdates ^ fun db -> 
            printfn "LOGX (2.1)"
            db.parseRequests 
            |> Set.fold (fun db html -> parse html db) { db with parseRequests = Set.empty }
            |> fun db -> printfn "LOGX (2.2) %O" db; db

module Routers =
    open Suave
    open Suave.Successful
    open Newtonsoft.Json
    type D = JoyReactor.SyncStore.Diff

    let callParser parser =
        request ^ fun r ->
            r.rawForm
            |> System.Text.Encoding.UTF8.GetString
            |> parser
            |> JsonConvert.SerializeObject
            |> OK

    let sync =
        request ^ fun r ->
            printfn "LOGX (1.1) | %A" (r.multiPartFields |> List.map fst)
            r.multiPartFields
            |> ParseDomain.sync
            |> fun (D.Diff bytes) -> ok bytes
            |> fun r -> printfn "LOGX (1.2) :: %O" r; r

open JoyReactor

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
module P = JoyReactor.Parsers

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
        POST >=> path "/sync" >=> Routers.sync
        POST >=> path "/messages" >=> Routers.callParser P.getMessages
        POST >=> path "/profile" >=> Routers.callParser P.profile
        POST >=> path "/toptags" >=> Routers.callParser P.parseTopTags
        POST >=> path "/tags" >=> Routers.callParser P.readTags
        POST >=> path "/post" >=> Routers.callParser P.parsePost
        POST >=> path "/posts" >=> Routers.callParser P.parsePostsWithNext ]
    |> startWebServer {
        defaultConfig with
            bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8080us ] }
    0
