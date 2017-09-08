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

module Page = Home

[<Pojo>]
type State =
    { model : Page.Model }

type PostComponent(props) =
    inherit React.Component<obj, State>(props)
    do base.setInitState { model = fst Page.init }

    member this.componentDidMount() = 
        promise {
            let model, cmd = Page.init
            this.setState { model = model }

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

    member this.render() = 
        Page.view this.state.model