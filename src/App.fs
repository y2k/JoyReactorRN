module Scenes

open Fable.Helpers.ReactNative
open Elmish
open Elmish.React
open Elmish.ReactNative
open Elmish.HMR

module TabsScreen =
    open Fable.Helpers.ReactNative.Props

    type Msg = 
        | SelectTab of int
        | HomeMsg of Home.Msg 
        | TagsMsg of TagsScreen.Msg
        | ThreadsMsg of ThreadsScreen.Msg
        | ProfileMsg of ProfileScreen.Msg
        | MessagesMsg of MessagesScreen.Msg
    type Model = 
        | HomeModel of Home.Model 
        | TagsModel of TagsScreen.Model
        | ThreadsModel of ThreadsScreen.Model
        | ProfileModel of ProfileScreen.Model
        | MessagesModel of MessagesScreen.Model

    let init = 
        Home.init |> fun (model, cmd) -> HomeModel model, Cmd.map HomeMsg cmd

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
        | MessagesMsg subMsg, MessagesModel subModel ->
            MessagesScreen.update subModel subMsg
            |> fun (m, cmd) -> MessagesModel m, Cmd.map MessagesMsg cmd
        | _ -> model, Cmd.none

    let private renderContent (model: Model) dispatch =
        match model with
        | HomeModel sm -> Home.view sm (HomeMsg >> dispatch)
        | TagsModel sm -> TagsScreen.view sm
        | ThreadsModel sm -> ThreadsScreen.view sm
        | ProfileModel sm -> ProfileScreen.view sm
        | MessagesModel sm -> MessagesScreen.view sm (MessagesMsg >> dispatch)
    
    let view model dispatch =
        let button title id =
            let nextButtonOutter =
                TouchableWithoutFeedbackProperties.Style 
                    [ Margin 2. 
                      BackgroundColor "#e49421"
                      BorderRadius 4.
                      Flex 1.
                      Height 48.
                      JustifyContent JustifyContent.Center
                      Overflow Overflow.Hidden ]
            let tabButtonInner =
                TextProperties.Style 
                    [ FontWeight FontWeight.Bold
                      FontSize 14.
                      TextAlign TextAlignment.Center
                      TextStyle.Color "white" ]
            touchableOpacity 
                [ nextButtonOutter
                  OnPress (fun _ -> SelectTab id |> dispatch) ]
                [ text [ tabButtonInner ] title ]

        view [ ViewProperties.Style [ Flex 1. ] ] [
            view [ ViewProperties.Style [ Flex 1. ] ] 
                 [ renderContent model dispatch ]
            view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding 2. ] ] 
                 [ button "Home" 0
                   button "Tags" 1
                   button "Messages" 2
                   button "Profile" 3 ] ]

module App =
    type Msg = 
        | TabsMsg of TabsScreen.Msg
        | HomeMsg of Home.Msg 
        | PostMsg of PostScreen.Msg 
        | OpenPost 
        | NavigateBack 
        | ProfileMsg of ProfileScreen.Msg 
        | LoginMsg of LoginScreen.Msg 
        | TagsMsg of TagsScreen.Msg
    type SubModel = 
        | TabsModel of TabsScreen.Model
        | HomeModel of Home.Model 
        | PostModel of PostScreen.Model 
        | ProfileModel of ProfileScreen.Model 
        | LoginModel of LoginScreen.Model 
        | TagsModel of TagsScreen.Model
    type Model = { subModel : SubModel; history : SubModel list }
    
    let init _ = TabsScreen.init 
               |> fun (model, cmd) -> { subModel = TabsModel model; history = [] }, Cmd.map TabsMsg cmd
    
    let update msg model : Model * Cmd<Msg> =
        let wrap ctor msgCtor model (subModel, cmd)  =
            { model with subModel = ctor subModel }, Cmd.map msgCtor cmd

        match msg, model.subModel with
        | NavigateBack, _ ->
            match model.history with
            | x :: xs -> { model with subModel = x ; history = xs }, Cmd.none
            | [] -> 
                exitApp()
                model, Cmd.none
        | TabsMsg (TabsScreen.HomeMsg (Home.OpenPost p)), _ ->
            PostScreen.init p.id
            |> fun (m, cmd) -> 
                { model with 
                    subModel = PostModel m
                    history = model.subModel :: model.history }, 
                Cmd.map PostMsg cmd
        | HomeMsg (Home.OpenPost p), _ -> 
            PostScreen.init p.id
            |> fun (m, cmd) -> 
                { model with 
                    subModel = PostModel m
                    history = model.subModel :: model.history }, 
                Cmd.map PostMsg cmd
        | TabsMsg subMsg, TabsModel subModel -> 
            TabsScreen.update subModel subMsg |> wrap TabsModel TabsMsg model
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

let setupBackHandler dispatch =    
    let backHandler () =
        dispatch App.Msg.NavigateBack
        true
    setOnHardwareBackPressHandler backHandler

let subscribe _ =
    Cmd.batch [Cmd.ofSub setupBackHandler]

Program.mkProgram App.init App.update App.view
|> Program.withSubscription subscribe
// #if RELEASE
// #else
|> Program.withConsoleTrace
|> Program.withHMR
// #endif
|> Program.withReactNative "joyreact"
|> Program.run        