module Scenes

open Fable.Core.JsInterop
open Fable.Import.React
open Fable.PowerPack
open Fable.Helpers.ReactNative
open Elmish
open JoyReactor.Utils

module TabsScreen =
    open Fable.Helpers.ReactNative.Props

    type Msg = 
        | SelectTab of int
        | HomeMsg of Home.Msg 
        | TagsMsg of TagsScreen.Msg
        | ThreadsMsg of ThreadsScreen.Msg
        | ProfileMsg of ProfileScreen.Msg
    type Model = 
        | HomeModel of Home.Model 
        | TagsModel of TagsScreen.Model
        | ThreadsModel of ThreadsScreen.Model
        | ProfileModel of ProfileScreen.Model

    let init = 
        ThreadsScreen.init |> fun (model, cmd) -> ThreadsModel model, Cmd.map ThreadsMsg cmd

    let update model msg : Model * Cmd<Msg> =
        match msg, model with
        | SelectTab 0, _ -> Home.init |> fun (model, cmd) -> HomeModel model, Cmd.map HomeMsg cmd
        | SelectTab 1, _ -> TagsScreen.init |> fun (model, cmd) -> TagsModel model, Cmd.map TagsMsg cmd
        | SelectTab 2, _ -> ThreadsScreen.init |> fun (model, cmd) -> ThreadsModel model, Cmd.map ThreadsMsg cmd
        | SelectTab 3, _ -> ProfileScreen.init |> fun (model, cmd) -> ProfileModel model, Cmd.map ProfileMsg cmd
        | HomeMsg subMsg, HomeModel subModel -> 
            Home.update subModel subMsg
            |> fun (m, cmd) -> HomeModel m, Cmd.map HomeMsg cmd
        | TagsMsg subMsg, TagsModel subModel -> 
            TagsScreen.update subModel subMsg
            |> fun (m, cmd) -> TagsModel m , Cmd.map TagsMsg cmd
        | ThreadsMsg subMsg, ThreadsModel subModel -> 
            ThreadsScreen.update subModel subMsg
            |> fun (m, cmd) -> ThreadsModel m , Cmd.map ThreadsMsg cmd
        | ProfileMsg subMsg, ProfileModel subModel ->
            ProfileScreen.update subModel subMsg
            |> fun (m, cmd) -> ProfileModel m, Cmd.map ProfileMsg cmd
        | _ -> model, Cmd.none

    let private renderContent (model: Model) dispatch =
        match model with
        | HomeModel sm -> Home.view sm (HomeMsg >> dispatch)
        | TagsModel sm -> TagsScreen.view sm
        | ThreadsModel sm -> ThreadsScreen.view sm
        | ProfileModel sm -> ProfileScreen.view sm
    
    let view model dispatch =
        let button title id =
            let nextButtonOutter =
                TouchableWithoutFeedbackProperties.Style 
                    [ Margin 4. 
                      BackgroundColor "#e49421"
                      BorderRadius 4.
                      Flex 1.
                      Height 48.
                      Overflow Overflow.Hidden ]
            let tabButtonInner =
                TextProperties.Style 
                    [ FontWeight FontWeight.Bold
                      FontSize 13.
                      TextAlign TextAlignment.Center
                      Padding 15.
                      Color "white" ]
            touchableOpacity 
                [ nextButtonOutter
                  OnPress (fun _ -> SelectTab id |> dispatch) ]
                [ text [ tabButtonInner ] title ]

        view [ ViewProperties.Style [ Flex 1. ] ] [
            view [ ViewProperties.Style [ Flex 1. ] ] 
                 [ renderContent model dispatch ]
            view [ ViewProperties.Style [ FlexDirection FlexDirection.Row ] ] [
                button "Home" 0
                button "Tags" 1
                button "Threads" 2
                button "Profile" 3 ] ]

module App =
    type Msg = 
        | TabsMsg of TabsScreen.Msg
        | HomeMsg of Home.Msg | PostMsg of PostScreen.Msg | OpenPost | NavigateBack | ProfileMsg of ProfileScreen.Msg | LoginMsg of LoginScreen.Msg | TagsMsg of TagsScreen.Msg
    type SubModel = 
        | TabsModel of TabsScreen.Model
        | HomeModel of Home.Model | PostModel of PostScreen.Model | ProfileModel of ProfileScreen.Model | LoginModel of LoginScreen.Model | TagsModel of TagsScreen.Model
    type Model = { subModel : SubModel; history : SubModel list }
    
    let init = TabsScreen.init 
               |> fun (model, cmd) -> { subModel = TabsModel model; history = [] }, Cmd.map TabsMsg cmd
    
    let update model msg : Model * Cmd<Msg> =
        match msg, model.subModel with
        | NavigateBack, _ ->
            match model.history with
            | x :: xs -> { model with subModel = x ; history = xs }, Cmd.none
            | [] -> 
                exitApp()
                model, Cmd.none
        | HomeMsg (Home.OpenPost p), _ -> 
            PostScreen.init p.id
            |> fun (m, cmd) -> 
                { model with 
                    subModel = PostModel m
                    history = model.subModel :: model.history }, 
                Cmd.map PostMsg cmd
        | TabsMsg subMsg, TabsModel subModel -> 
            TabsScreen.update subModel subMsg
            |> fun (m, cmd) -> { model with subModel = TabsModel m }, Cmd.map TabsMsg cmd
        | HomeMsg subMsg, HomeModel subModel -> 
            Home.update subModel subMsg
            |> fun (m, cmd) -> { model with subModel = HomeModel m }, Cmd.map HomeMsg cmd
        | PostMsg subMsg, PostModel subModel -> 
            PostScreen.update subModel subMsg
            |> fun (m, cmd) -> { model with subModel = PostModel m }, Cmd.map PostMsg cmd
        | ProfileMsg subMsg, ProfileModel subModel ->
            ProfileScreen.update subModel subMsg
            |> fun (m, cmd) -> { model with subModel = ProfileModel m }, Cmd.map ProfileMsg cmd
        | TagsMsg subMsg, TagsModel subModel ->
            TagsScreen.update subModel subMsg
            |> fun (m, cmd) -> { model with subModel = TagsModel m }, Cmd.map TagsMsg cmd
        | LoginMsg subMsg, LoginModel subModel ->
            LoginScreen.update subModel subMsg
            |> fun (m, cmd) -> { model with subModel = LoginModel m }, Cmd.map LoginMsg cmd
        | _ -> model, Cmd.none

    let view model dispatch =
        match model.subModel with
        | HomeModel subModel -> Home.view subModel (HomeMsg >> dispatch)
        | PostModel subModel -> PostScreen.view subModel (PostMsg >> dispatch)
        | ProfileModel subModel -> ProfileScreen.view subModel
        | LoginModel subModel -> LoginScreen.view subModel (LoginMsg >> dispatch)
        | TagsModel subModel -> TagsScreen.view subModel
        | TabsModel sm -> TabsScreen.view sm (TabsMsg >> dispatch)

type PostComponent(props) =
    inherit Component<obj, State<App.Model>>(props)
    do base.setInitState { model = fst App.init }

    member this.componentDidMount() = 
        setOnHardwareBackPressHandler
            (fun _ -> Cmd.dispatch this App.update (Cmd.ofMsg App.NavigateBack); true)
        promise {
            let font = import "Font" "expo"
            do! !!font?loadAsync(createObj [ "icomoon" ==> require("../assets/fonts/icomoon.ttf") ])

            let model, cmd = App.init
            this.setState { model = model }
            Cmd.dispatch this App.update cmd
        } |> Promise.start

    member this.render() : ReactElement = 
        App.view this.state.model (Cmd.ofMsg >> (Cmd.dispatch this App.update))