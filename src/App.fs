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

module App =
    type Msg = HomeMsg of Home.Msg | PostMsg of PostScreen.Msg | OpenPost
    type SubModel = HomeModel of Home.Model | PostModel of PostScreen.Model
    type Model = { subModel : SubModel }
    let init = Home.init |> fun (model, cmd) -> { subModel = HomeModel model }, Cmd.map HomeMsg cmd
    let update model msg : Model * Cmd<Msg> =
        match msg, model.subModel with
        | HomeMsg (Home.OpenPost _), _ -> model, Cmd.none // FIXME: Open post
        | HomeMsg subMsg, HomeModel subModel -> 
            Home.update subModel subMsg
            |> fun (m, cmd) -> { subModel = HomeModel m }, Cmd.map HomeMsg cmd
        | PostMsg subMsg, PostModel subModel -> 
            PostScreen.update subModel subMsg
            |> fun (m, cmd) -> { subModel = PostModel m }, Cmd.map PostMsg cmd
        | _ -> model, Cmd.none
    let view model dispatch =
        match model.subModel with
        | HomeModel subModel -> Home.view subModel (HomeMsg >> dispatch)
        | PostModel subModel -> PostScreen.view subModel (PostMsg >> dispatch)

type PostComponent(props) =
    inherit React.Component<obj, State<App.Model>>(props)
    do base.setInitState { model = fst App.init }

    member this.componentDidMount() = 
        promise {
            let font = import "Font" "expo"
            do! !!font?loadAsync(createObj [ "icomoon" ==> require("../assets/fonts/icomoon.ttf") ])

            let model, cmd = App.init
            this.setState { model = model }
            Cmd.dispatch this App.update cmd
        } |> Promise.start

    member this.render() : ReactElement = 
        App.view this.state.model (Cmd.ofMsg >> (Cmd.dispatch this App.update))