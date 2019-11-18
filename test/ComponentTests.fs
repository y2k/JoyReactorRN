module ComponentTests

open JoyReactor.Types
open Xunit
open JR.Screens
module A = JoyReactor.Services.ApiRequests
module S = JoyReactor.SyncStore

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let private RESOURCES_DIR = sprintf "%s/Resources" __SOURCE_DIRECTORY__

module Utils =
    let init() =
        A.downloadString :=
            fun url _ _ ->
                match url with
                | Regex @"http://joyreactor.cc/(\d+)" [ page ] -> sprintf "feed_%s.html" page
                | Regex @"/post/(\d+)" [ id ] -> sprintf "post_%s.html" id
                | "http://joyreactor.cc/" -> "feed.html"
                | _ -> failwith url
                |> sprintf "%s/%s" RESOURCES_DIR
                |> System.IO.File.ReadAllText
                |> async.Return
        S.sendToServer := fun _ -> async.Return []

        let f = S.sub.[0]
        f ^ fun db ->
                let newDb = ParseDomain.handleDbChange db
                if db <> newDb
                    then S.dispatch ParseDomain.handleDbChange |> Async.RunSynchronously

    let private runCmd cmd =
        let mutable msg: _ list = []
        cmd |> List.iter ^ fun f -> f (fun x -> msg <- x :: msg)
        msg
    let readDb sub =
        S.update (fun db -> db, sub db) |> Async.RunSynchronously
    let private runUpdate msgs model update =
        msgs
        |> List.fold (
            fun (model, cmds) msg ->
                let (m, c) = update model msg
                m, c :: cmds)
            (model, [])
        |> fun (model, cmds) -> model, List.concat cmds
    let runUpdateCmd update model msgs =
        runUpdate msgs model update
        |> fun (model, cmd) -> model, runCmd cmd
    let runInitCmd init =
        init
        |> fun (model, cmd) -> model, runCmd cmd

open Swensen.Unquote

[<Fact>]
let ``feed test``() =
    Utils.init()

    let (model, msgs) = Utils.runInitCmd (FeedScreen.init FeedSource)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs // PostsLoadedFromCache
    test <@ model.items = [||] && not model.hasNew @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs // FirstPagePreloaded
    test <@ msgs = [] && model.items = [||] && model.hasNew @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model [ FeedScreen.ApplyPreloaded ]
    test <@ model.items = [||] && not model.hasNew @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs
    test <@ msgs = [] && Seq.length model.items = 11 && not model.hasNew @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model [ FeedScreen.LoadNextPage ]
    test <@ Seq.length model.items = 11 && not model.hasNew && model.loading @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs
    test <@ msgs = [] && not model.hasNew && model.items.Length = 21 && not model.loading @>
    
    (* Refresh list *)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model [ FeedScreen.Refresh ]
    test <@ model.items.Length = 21 && not model.hasNew && model.loading @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs
    test <@ msgs = [] && model.items.Length = 11 && not model.hasNew && not model.loading @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model [ FeedScreen.LoadNextPage ]
    test <@ model.items.Length = 11 && not model.hasNew && model.loading @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs
    test <@ msgs = [] && not model.hasNew && model.items.Length = 21 && not model.loading @>

    (* Second enter to screen *)

    let (model, msgs) = Utils.runInitCmd (FeedScreen.init FeedSource)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs // PostsLoadedFromCache
    test <@ model.items.Length = 20 && not model.hasNew && model.loading @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs // FirstPagePreloaded
    test <@ msgs = [] && model.items.Length = 20 && model.hasNew && not model.loading @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model [ FeedScreen.ApplyPreloaded ]
    test <@ model.items.Length = 20 && not model.hasNew @>

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs
    test <@ msgs = [] && model.items.Length = 21 && not model.hasNew @>

[<Fact>]
let ``tags test``() =
    Utils.init()

    let (model, msgs) = Utils.runInitCmd TagsScreen.init

    let (model, msgs) = Utils.runUpdateCmd TagsScreen.update model msgs
    test <@ Seq.length msgs = 2 @>

    let (model, msgs) = Utils.runUpdateCmd TagsScreen.update model msgs
    test <@ Seq.isEmpty msgs @>

    let msg = Utils.readDb TagsScreen.sub
    let (model, _) = TagsScreen.update model msg
    test <@ Seq.length model.tags = 11 @>

[<Fact>]
let ``post test``() =
    Utils.init()

    let id = 4111388

    let (model, msgs) = Utils.runInitCmd (PostScreen.init id)
    test <@ model = { PostScreen.Model.empty with id = id } @>

    let (model, _) = Utils.runUpdateCmd PostScreen.update model msgs
    test <@ model = { PostScreen.Model.empty with id = id } @>

    let msg = Utils.readDb (PostScreen.sub id)
    let (model, _) = Utils.runUpdateCmd PostScreen.update model [ msg ]
    Assert.Equal (
         [| "семья"; "уведомление"; "дети" |] |> Seq.toList,
         model.post |> Option.map (fun x -> x.tags) |> Option.get)
