module ComponentTests

open Xunit
module A = JoyReactor.Services.ApiRequests
module S = JoyReactor.SyncStore

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let private RESOURCES_DIR = sprintf "%s/Resources" __SOURCE_DIRECTORY__

module Utils =
    let init () =
        A.downloadString :=
            fun url _ _ ->
                let id = match url with | Regex @"/(\d+)" [ id ] -> id | _ -> failwith url
                sprintf "%s/post_%s.html" RESOURCES_DIR id
                |> System.IO.File.ReadAllText
                |> async.Return
        S.sendToServer := fun _ -> async.Return []
    let runCmd' cmd =
        let mutable msg : _ list = []
        cmd |> List.iter ^ fun f -> f (fun x -> msg <- x :: msg)
        S.dispatch ParseDomain.handleDbChange |> Async.RunSynchronously
        msg
    [<System.Obsolete>]
    let runCmd cmd =
        let mutable msg = None
        cmd |> List.iter ^ fun f -> f (fun x -> msg <- Some x)
        S.dispatch ParseDomain.handleDbChange |> Async.RunSynchronously
        msg |> Option.get
    let readDb sub =
        S.update (fun db -> db, sub db) |> Async.RunSynchronously
    let runUpdate msgs model update =
        msgs
        |> List.fold (
            fun (model, cmds) msg -> 
                let (m, c) = update model msg
                m, c :: cmds) 
            (model, [])
        |> fun (model, cmds) -> model, List.concat cmds
    let runUpdateCmd update model msgs =
        runUpdate msgs model update
        |> fun (model, cmd) -> model, runCmd' cmd

[<Fact>]
let ``tags test``() =
    Utils.init ()

    let (model, msgs) = 
        TagsScreen.init
        |> fun (model, cmd) -> model, Utils.runCmd' cmd

    let (model, msgs) = Utils.runUpdateCmd TagsScreen.update model msgs
    Assert.Equal(2, Seq.length msgs)

    let (model, msgs) = Utils.runUpdateCmd TagsScreen.update model msgs
    Assert.Equal(0, Seq.length msgs)

    let msg = Utils.readDb TagsScreen.sub
    let (model, cmd) = TagsScreen.update model msg

    Assert.Equal(obj(), Seq.length model.tags)

[<Fact>]
let ``post test``() =
    A.downloadString :=
        fun url _ _ ->
            let id = match url with | Regex @"/(\d+)" [ id ] -> id | _ -> failwith url
            sprintf "%s/post_%s.html" RESOURCES_DIR id
            |> System.IO.File.ReadAllText
            |> async.Return
    S.sendToServer := fun _ -> async.Return []

    let id = 4111388

    let (model, cmd) = PostScreen.init id
    Assert.Equal({ PostScreen.Model.empty with id = id }, model)
    Assert.Equal(1, Seq.length cmd)

    let msg = Utils.runCmd cmd
    let (model, cmd) = PostScreen.update model msg
    Assert.StrictEqual([], cmd)
    Assert.Equal({ PostScreen.Model.empty with id = id }, model)

    let msg = Utils.readDb (PostScreen.sub id)
    let (model, cmd) = PostScreen.update model msg
    Assert.StrictEqual([], cmd)
    Assert.Equal (
         [| "семья"; "уведомление"; "дети" |] |> Seq.toList,
         model.post |> Option.map (fun x -> x.tags) |> Option.get)
