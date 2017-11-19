module Elmish

open Fable.Core
open Fable.Import.JS
open Fable.Import
open Fable.PowerPack

[<Pojo>]
type State<'a> = { model : 'a }

type Cmd<'a> =
| EmptyCommand
| PromiseCommand of Promise<'a>
| MsgCommand of 'a
    static member map (ctr : 'a -> 'b) (cmd : Cmd<'a>) : Cmd<'b> = 
        match cmd with
        | EmptyCommand -> EmptyCommand
        | MsgCommand x -> MsgCommand (ctr x)
        | PromiseCommand p -> p |> Fable.PowerPack.Promise.map ctr |> PromiseCommand

    static member ofMsg m : Cmd<'a> = MsgCommand m
    static member none : Cmd<'a> = EmptyCommand
    static member ofPromise (p : Promise<'a>) (okMessage: Result<'a, string> -> 'b) =
        promise {
            try
                let! result = p
                return okMessage <| Ok result
            with
            | e -> 
                return okMessage <| Result.Error (string e.Message)
        } |> PromiseCommand
    static member dispatch (this : React.Component<obj, State<'a>>) update cmd =
        promise {
            let mutable doWhile = true
            let mutable currentCmd = cmd
            while doWhile do
                match currentCmd with
                | EmptyCommand -> 
                    doWhile <- false
                | MsgCommand msg -> 
                    let model2, cmd2 = update this.state.model msg
                    this.setState { model = model2 }
                    currentCmd <- cmd2
                | PromiseCommand msgPromise -> 
                    let! msg = msgPromise
                    let model2, cmd2 = update this.state.model msg
                    this.setState { model = model2 }
                    currentCmd <- cmd2
        } |> Fable.PowerPack.Promise.start