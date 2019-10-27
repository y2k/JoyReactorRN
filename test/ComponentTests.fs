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
    let runCmd cmd =
        let mutable msg = None
        cmd |> List.iter ^ fun f -> f (fun x -> msg <- Some x)
        S.dispatch ParseDomain.handleDbChange |> Async.RunSynchronously
        msg |> Option.get

[<Fact>]
let ``tags test``() =
    let (model, cmd) = TagsScreen.init
    Assert.Equal(TagsScreen.Model.empty, model)

    let msg = Utils.runCmd cmd
    printfn "%O" msg
    ()

[<Fact>]
let ``post test``() =
    let readDb sub =
        S.update (fun db -> db, sub db) |> Async.RunSynchronously

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

    let msg = readDb (PostScreen.sub id)
    let (model, cmd) = PostScreen.update model msg
    Assert.StrictEqual([], cmd)
    Assert.Equal (
         [| "семья"; "уведомление"; "дети" |] |> Seq.toList,
         model.post |> Option.map (fun x -> x.tags) |> Option.get)
