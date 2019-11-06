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

[<Fact>]
let ``feed test``() =
    Utils.init()

    let (model, msgs) = Utils.runInitCmd (FeedScreen.init FeedSource)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs // PostsLoadedFromCache
    Assert.Equal([], model.items)
    Assert.Equal(false, model.hasNew)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs // FirstPagePreloaded
    Assert.Equal([||], msgs)
    Assert.Equal([], model.items)
    Assert.Equal(true, model.hasNew)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model [ FeedScreen.ApplyPreloaded ]
    Assert.Equal([], model.items)
    Assert.Equal(false, model.hasNew)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs
    Assert.Equal([||], msgs)
    Assert.Equal(10, Seq.length model.items)
    Assert.Equal(false, model.hasNew)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model [ FeedScreen.LoadNextPage ]
    Assert.Equal(10, Seq.length model.items)
    Assert.Equal(false, model.hasNew)
    Assert.Equal(true, model.loading)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs
    Assert.Equal([||], msgs)
    Assert.Equal(false, model.hasNew)
    Assert.Equal(20, Seq.length model.items)
    Assert.Equal(false, model.loading)
    
    (* Refresh list *)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model [ FeedScreen.Refresh ]
    Assert.Equal(20, Seq.length model.items)
    Assert.Equal(false, model.hasNew)
    Assert.Equal(true, model.loading)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs
    Assert.Equal([||], msgs)
    Assert.Equal(11, Seq.length model.items)
    Assert.Equal(false, model.hasNew)
    Assert.Equal(false, model.loading)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model [ FeedScreen.LoadNextPage ]
    Assert.Equal(11, Seq.length model.items)
    Assert.Equal(false, model.hasNew)
    Assert.Equal(true, model.loading)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs
    Assert.Equal([||], msgs)
    Assert.Equal(false, model.hasNew)
    Assert.Equal(21, Seq.length model.items)
    Assert.Equal(false, model.loading)

    (* Second enter to screen *)

    let (model, msgs) = Utils.runInitCmd (FeedScreen.init FeedSource)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs // PostsLoadedFromCache
    Assert.Equal(20, Seq.length model.items)
    Assert.Equal(false, model.hasNew)
    Assert.Equal(true, model.loading)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs // FirstPagePreloaded
    Assert.Equal([||], msgs)
    Assert.Equal(20, Seq.length model.items)
    Assert.Equal(true, model.hasNew)
    Assert.Equal(false, model.loading)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model [ FeedScreen.ApplyPreloaded ]
    Assert.Equal(20, Seq.length model.items)
    Assert.Equal(false, model.hasNew)

    let (model, msgs) = Utils.runUpdateCmd FeedScreen.update model msgs
    Assert.Equal([||], msgs)
    Assert.Equal(21, Seq.length model.items)
    Assert.Equal(false, model.hasNew)

[<Fact>]
let ``tags test``() =
    Utils.init()

    let (model, msgs) = Utils.runInitCmd TagsScreen.init

    let (model, msgs) = Utils.runUpdateCmd TagsScreen.update model msgs
    Assert.Equal(2, Seq.length msgs)

    let (model, msgs) = Utils.runUpdateCmd TagsScreen.update model msgs
    Assert.Equal(0, Seq.length msgs)

    let msg = Utils.readDb TagsScreen.sub
    let (model, _) = TagsScreen.update model msg

    Assert.Equal(11, Seq.length model.tags)

[<Fact>]
let ``post test``() =
    Utils.init()

    let id = 4111388

    let (model, msgs) = Utils.runInitCmd (PostScreen.init id)
    Assert.Equal({ PostScreen.Model.empty with id = id }, model)

    let (model, _) = Utils.runUpdateCmd PostScreen.update model msgs
    Assert.Equal({ PostScreen.Model.empty with id = id }, model)

    let msg = Utils.readDb (PostScreen.sub id)
    let (model, _) = Utils.runUpdateCmd PostScreen.update model [ msg ]
    Assert.Equal (
         [| "семья"; "уведомление"; "дети" |] |> Seq.toList,
         model.post |> Option.map (fun x -> x.tags) |> Option.get)
