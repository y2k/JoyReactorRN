module Scenes

open Fable.Helpers.ReactNative
open Elmish
open Elmish.React
open Elmish.ReactNative
open Elmish.HMR
open JoyReactor.CommonUi

module TabsScreen =
    open Fable.Helpers.ReactNative.Props
    open JoyReactor
    open JoyReactor.Types
    
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
    
    let init = Home.init FeedSource |> fun (model, cmd) -> HomeModel model, Cmd.map HomeMsg cmd
    
    let update model msg: Model * Cmd<Msg> =
        match msg, model with
        | SelectTab 0, _ -> Home.init FeedSource |> fun (model, cmd) -> HomeModel model, Cmd.map HomeMsg cmd
        | SelectTab 1, _ -> TagsScreen.init |> fun (model, cmd) -> TagsModel model, Cmd.map TagsMsg cmd
        | SelectTab 2, _ -> ThreadsScreen.init |> fun (model, cmd) -> ThreadsModel model, Cmd.map ThreadsMsg cmd
        | SelectTab 3, _ -> ProfileScreen.init |> fun (model, cmd) -> ProfileModel model, Cmd.map ProfileMsg cmd
        | HomeMsg subMsg, HomeModel subModel -> 
            Home.update subModel subMsg |> fun (m, cmd) -> HomeModel m, Cmd.map HomeMsg cmd
        | TagsMsg subMsg, TagsModel subModel -> 
            TagsScreen.update subModel subMsg |> fun (m, cmd) -> TagsModel m, Cmd.map TagsMsg cmd
        | ThreadsMsg subMsg, ThreadsModel subModel -> 
            ThreadsScreen.update subModel subMsg |> fun (m, cmd) -> ThreadsModel m, Cmd.map ThreadsMsg cmd
        | ProfileMsg subMsg, ProfileModel subModel -> 
            ProfileScreen.update subModel subMsg |> fun (m, cmd) -> ProfileModel m, Cmd.map ProfileMsg cmd
        | MessagesMsg subMsg, MessagesModel subModel -> 
            MessagesScreen.update subModel subMsg |> fun (m, cmd) -> MessagesModel m, Cmd.map MessagesMsg cmd
        | _ -> model, Cmd.none
    
    let private renderContent (model: Model) dispatch =
        match model with
        | HomeModel sm -> Home.view sm (HomeMsg >> dispatch)
        | TagsModel sm -> TagsScreen.view sm (TagsMsg >> dispatch)
        | ThreadsModel sm -> ThreadsScreen.view sm (ThreadsMsg >> dispatch)
        | ProfileModel sm -> ProfileScreen.view sm (ProfileMsg >> dispatch)
        | MessagesModel sm -> MessagesScreen.view sm (MessagesMsg >> dispatch)
    
    let view model dispatch =
        view [ ViewProperties.Style [ Flex 1. ] ] 
            [ view [ ViewProperties.Style [ Flex 1. ] ] [ renderContent model dispatch ]
              
              view [ ViewProperties.Style [ FlexDirection FlexDirection.Row
                                            Padding 2. ] ] 
                  [ roundButton "Home" (dispatch <! SelectTab 0) [ Flex 1. ]
                    roundButton "Tags" (dispatch <! SelectTab 1) [ Flex 1. ]
                    roundButton "Messages" (dispatch <! SelectTab 2) [ Flex 1. ]
                    roundButton "Profile" (dispatch <! SelectTab 3) [ Flex 1. ] ] ]

module App =
    type Msg =
        | TabsMsg of TabsScreen.Msg
        | HomeMsg of Home.Msg
        | PostMsg of PostScreen.Msg
        | MessagesMsg of MessagesScreen.Msg
        | OpenPost
        | NavigateBack
        | ProfileMsg of ProfileScreen.Msg
        | LoginMsg of LoginScreen.Msg
        | TagsMsg of TagsScreen.Msg
    
    type SubModel =
        | TabsModel of TabsScreen.Model
        | HomeModel of Home.Model
        | PostModel of PostScreen.Model
        | MessagesModel of MessagesScreen.Model
        | ProfileModel of ProfileScreen.Model
        | LoginModel of LoginScreen.Model
        | TagsModel of TagsScreen.Model
    
    type Model =
        { subModel: SubModel
          history: SubModel list }
    
    let init _ =
        TabsScreen.init
        |> fun (model, cmd) -> 
            { subModel = TabsModel model
              history = [] }, Cmd.map TabsMsg cmd
    
    let update msg model: Model * Cmd<Msg> =
        let wrap ctor msgCtor model (subModel, cmd) = { model with subModel = ctor subModel }, Cmd.map msgCtor cmd
        match msg, model.subModel with
        | NavigateBack, _ -> 
            match model.history with
            | x :: xs -> 
                { model with subModel = x
                             history = xs }, Cmd.none
            | [] -> 
                exitApp()
                model, Cmd.none
        | TabsMsg(TabsScreen.HomeMsg(Home.OpenPost p)), _ -> 
            PostScreen.init p.id |> wrap PostModel PostMsg { model with history = model.subModel :: model.history }
        | HomeMsg(Home.OpenPost p), _ -> 
            PostScreen.init p.id |> wrap PostModel PostMsg { model with history = model.subModel :: model.history }
        | TabsMsg(TabsScreen.TagsMsg(TagsScreen.OpenPosts p)), _ -> 
            Home.init p |> wrap HomeModel HomeMsg { model with history = model.subModel :: model.history }
        | TabsMsg(TabsScreen.ThreadsMsg(ThreadsScreen.ThreadSelected id)), _ -> 
            MessagesScreen.init id 
            |> wrap MessagesModel MessagesMsg { model with history = model.subModel :: model.history }
        | PostMsg(PostScreen.OpenTag source), _ -> 
            Home.init source |> wrap HomeModel HomeMsg { model with history = model.subModel :: model.history }
        | PostMsg subMsg, PostModel subModel -> PostScreen.update subModel subMsg |> wrap PostModel PostMsg model
        | MessagesMsg subMsg, MessagesModel subModel -> 
            MessagesScreen.update subModel subMsg |> wrap MessagesModel MessagesMsg model
        | TabsMsg subMsg, TabsModel subModel -> TabsScreen.update subModel subMsg |> wrap TabsModel TabsMsg model
        | HomeMsg subMsg, HomeModel subModel -> Home.update subModel subMsg |> wrap HomeModel HomeMsg model
        | _ -> model, Cmd.none
    
    let view model dispatch =
        match model.subModel with
        | HomeModel subModel -> Home.view subModel (HomeMsg >> dispatch)
        | PostModel subModel -> PostScreen.view subModel (PostMsg >> dispatch)
        | ProfileModel subModel -> ProfileScreen.view subModel (ProfileMsg >> dispatch)
        | LoginModel subModel -> LoginScreen.view subModel (LoginMsg >> dispatch)
        | TagsModel subModel -> TagsScreen.view subModel (TagsMsg >> dispatch)
        | TabsModel subModel -> TabsScreen.view subModel (TabsMsg >> dispatch)
        | MessagesModel subModel -> MessagesScreen.view subModel (MessagesMsg >> dispatch)

let setupBackHandler dispatch =
    let backHandler() =
        dispatch App.Msg.NavigateBack
        true
    setOnHardwareBackPressHandler backHandler

let subscribe _ = Cmd.batch [ Cmd.ofSub setupBackHandler ]

Program.mkProgram App.init App.update App.view
|> Program.withSubscription subscribe
// #if RELEASE
// #else
|> Program.withConsoleTrace
|> Program.withHMR
// #endif
|> Program.withReactNative "joyreact"
|> Program.run
