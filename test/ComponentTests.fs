module ComponentTests

open Xunit
open PostScreen

[<Fact>]
let ``post test`` () =
    let (_, cmd) = PostScreen.init 4111388
    let mutable msg : Msg option = None
    cmd |> List.iter ^ fun f -> f (fun x -> msg <- Some x)
    
    match msg with
    | Some (RefreshComplete _) -> ()
    | actual -> failwithf "%O" actual
