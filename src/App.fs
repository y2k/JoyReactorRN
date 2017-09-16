module Scenes

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.React
open Fable.Import.ReactNative
open Fable.PowerPack

open Fable.Helpers.ReactNative.Props
open JoyReactor
open Elmish

[<Emit("require($0)")>]
let require (path: string) = jsNative

module Page = Home

[<Pojo>]
type State =
    { model : Page.Model }

type PostComponent(props) =
    inherit React.Component<obj, State>(props)
    do base.setInitState { model = fst Page.init }

    member private this.dispatch (cmd: Cmd<Page.Msg>) =
        promise {
            let mutable doWhile = true
            let mutable currentCmd = cmd
            while doWhile do
                match currentCmd with
                | EmptyCommand -> 
                    doWhile <- false
                | MsgCommand msg -> 
                    let model2, cmd2 = Page.update this.state.model msg
                    this.setState { model = model2 }
                    currentCmd <- cmd2
                | PromiseCommand msgPromise -> 
                    let! msg = msgPromise
                    let model2, cmd2 = Page.update this.state.model msg
                    this.setState { model = model2 }
                    currentCmd <- cmd2
        } |> Promise.start

    member this.componentDidMount() = 
        promise {
            let font = import "Font" "expo"
            do! !!font?loadAsync(createObj [ "icomoon" ==> require("../assets/fonts/icomoon.ttf") ])

            let model, cmd = Page.init
            this.setState { model = model }
            this.dispatch cmd
        } |> Promise.start

    member this.render() : ReactElement = 
        Page.view this.state.model (Cmd.ofMsg >> this.dispatch)