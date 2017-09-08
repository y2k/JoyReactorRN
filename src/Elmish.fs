module Elmish

open Fable.Import.JS
open Fable.PowerPack

let dispatch _ unit = ()

type Cmd<'a> =
| EmptyCommand
| PromiseCommand of Promise<'a>
| MsgCommand of 'a
    static member ofMsg m : Cmd<'a> = MsgCommand m
    static member none : Cmd<'a> = EmptyCommand
    static member ofPromise (p : Promise<'a>) (okMessage: Result<'a, string> -> 'b) =
        promise {
            try
                let! result = p
                return okMessage <| Ok result
            with
            | _ -> 
                return okMessage <| Result.Error "todo error"
        }
        |> PromiseCommand