module Scenes

open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.ReactNative
open Fable.ReactNative.Helpers
open JoyReactor

open Fetch

open JoyReactor.CommonUi
type LocalDb = JoyReactor.CofxStorage.LocalDb

module TabsScreen =
    open Fable.ReactNative.Props
    open JoyReactor.Types

    type Msg =
        | SelectTab of int
        | HomeMsg of PostsComponent.Msg
        | TagsMsg of TagsScreen.Msg
        | ThreadsMsg of ThreadsScreen.Msg
        | ProfileMsg of ProfileScreen.Msg
        | MessagesMsg of MessagesScreen.Msg
        | SubMsg of LocalDb

    type Model =
        | HomeModel of PostsComponent.Model * (LocalDb -> PostsComponent.Msg)
        | TagsModel of TagsScreen.Model
        | ThreadsModel of ThreadsScreen.Model
        | ProfileModel of ProfileScreen.Model
        | MessagesModel of MessagesScreen.Model

    let sub (db : LocalDb) = SubMsg db

    let init = 
        PostsComponent.init FeedSource 
        |> fun (model, cmd) -> HomeModel (model, fun x -> PostsComponent.sub FeedSource x), Cmd.map HomeMsg cmd

    let update model msg : Model * Cmd<Msg> =
        match msg, model with
        | SubMsg db, _ ->
            printfn "LOGX :: Tabs :: SubMsg"
            match model with
            | HomeModel (_,sub) -> model, Cmd.ofMsg ^ HomeMsg ^ sub db
            | TagsModel _ -> model, Cmd.ofMsg ^ TagsMsg ^ TagsScreen.sub db
            | ThreadsModel _ -> model, Cmd.ofMsg ^ ThreadsMsg ^ ThreadsScreen.sub db
            | _ -> model, Cmd.none
        | SelectTab 0, _ -> PostsComponent.init FeedSource |> fun (model, cmd) -> HomeModel (model, fun x -> PostsComponent.sub FeedSource x), Cmd.map HomeMsg cmd
        | SelectTab 1, _ -> TagsScreen.init |> fun (model, cmd) -> TagsModel model, Cmd.map TagsMsg cmd
        | SelectTab 2, _ -> ThreadsScreen.init |> fun (model, cmd) -> ThreadsModel model, Cmd.map ThreadsMsg cmd
        | SelectTab 3, _ -> ProfileScreen.init |> fun (model, cmd) -> ProfileModel model, Cmd.map ProfileMsg cmd
        | HomeMsg subMsg, HomeModel (subModel,s) ->
            PostsComponent.update subModel subMsg |> fun (m, cmd) -> HomeModel (m,s), Cmd.map HomeMsg cmd
        | TagsMsg subMsg, TagsModel subModel ->
            TagsScreen.update subModel subMsg |> fun (m, cmd) -> TagsModel m, Cmd.map TagsMsg cmd
        | ThreadsMsg subMsg, ThreadsModel subModel ->
            ThreadsScreen.update subModel subMsg |> fun (m, cmd) -> ThreadsModel m, Cmd.map ThreadsMsg cmd
        | ProfileMsg subMsg, ProfileModel subModel ->
            ProfileScreen.update subModel subMsg |> fun (m, cmd) -> ProfileModel m, Cmd.map ProfileMsg cmd
        | MessagesMsg subMsg, MessagesModel subModel ->
            MessagesScreen.update subModel subMsg |> fun (m, cmd) -> MessagesModel m, Cmd.map MessagesMsg cmd
        | _ -> model, Cmd.none

    let private renderContent (model : Model) dispatch =
        match model with
        | HomeModel (sm,_) -> PostsComponent.view sm (HomeMsg >> dispatch)
        | TagsModel sm -> TagsScreen.view sm (TagsMsg >> dispatch)
        | ThreadsModel sm -> ThreadsScreen.view sm (ThreadsMsg >> dispatch)
        | ProfileModel sm -> ProfileScreen.view sm (ProfileMsg >> dispatch)
        | MessagesModel sm -> MessagesScreen.view sm (MessagesMsg >> dispatch)

    let view model dispatch =
        view [ ViewProperties.Style [ Flex 1. ] ]
            [ view [ ViewProperties.Style [ Flex 1. ] ] [ renderContent model dispatch ]
              view [ ViewProperties.Style [ FlexDirection FlexDirection.Row
                                            Padding $ 2. ] ]
                  [ roundButton "Home" (dispatch <! SelectTab 0) [ Flex 1. ]
                    roundButton "Tags" (dispatch <! SelectTab 1) [ Flex 1. ]
                    roundButton "Messages" (dispatch <! SelectTab 2) [ Flex 1. ]
                    roundButton "Profile" (dispatch <! SelectTab 3) [ Flex 1. ] ] ]

module App =
    type Msg =
        | TabsMsg of TabsScreen.Msg
        | HomeMsg of PostsComponent.Msg
        | PostMsg of PostScreen.Msg
        | MessagesMsg of MessagesScreen.Msg
        | OpenPost
        | NavigateBack
        | ProfileMsg of ProfileScreen.Msg
        | LoginMsg of LoginScreen.Msg
        | TagsMsg of TagsScreen.Msg
        | LocalDbMsg of LocalDb

    type SubModel =
        | TabsModel of TabsScreen.Model
        | HomeModel of PostsComponent.Model
        | PostModel of PostScreen.Model
        | MessagesModel of MessagesScreen.Model
        | ProfileModel of ProfileScreen.Model
        | LoginModel of LoginScreen.Model
        | TagsModel of TagsScreen.Model

    type Model =
        { subModel : SubModel
          history : SubModel list
          subHistory : (LocalDb -> Msg) list }

    let sub = JoyReactor.SyncStore.sub |> Cmd.map LocalDbMsg

    let init _ =
        TabsScreen.init
        |> fun (model, cmd) ->
            { subModel = TabsModel model
              history = []
              subHistory = [ (fun db -> TabsScreen.sub db |> TabsMsg) ] }, Cmd.map TabsMsg cmd

    let update msg model : Model * Cmd<Msg> =
        let wrap ctor msgCtor model (subModel, cmd) = { model with subModel = ctor subModel }, Cmd.map msgCtor cmd
        match msg, model.subModel with
        | LocalDbMsg db, _ ->
            printfn "LOGX :: App :: LocalDbMsg | %O" db
            model,
            match model.subHistory with
            | f :: _ -> Cmd.ofMsg <| f db
            | _ -> Cmd.Empty
        | NavigateBack, _ ->
            match model.history with
            | x :: xs -> { model with subModel = x; history = xs }, Cmd.none
            | [] ->
                exitApp()
                model, Cmd.none
        | TabsMsg(TabsScreen.HomeMsg(PostsComponent.OpenPost p)), _ ->
            PostScreen.init p.id
            |> wrap PostModel PostMsg
                    { model with
                        subHistory = (fun db -> PostScreen.sub p.id db |> PostMsg) :: model.subHistory
                        history = model.subModel :: model.history }
        | TabsMsg(TabsScreen.ProfileMsg(ProfileScreen.Login)), _ ->
            LoginScreen.init
            |> wrap LoginModel LoginMsg
                    { model with
                        subHistory = (fun db -> LoginScreen.sub db |> LoginMsg) :: model.subHistory
                        history = model.subModel :: model.history }
        | HomeMsg(PostsComponent.OpenPost p), _ ->
            PostScreen.init p.id
            |> wrap PostModel PostMsg
                { model with
                    subHistory = (fun db -> PostScreen.sub p.id db |> PostMsg) :: model.subHistory
                    history = model.subModel :: model.history }
        | TabsMsg(TabsScreen.TagsMsg(TagsScreen.OpenPosts p)), _ ->
            PostsComponent.init p |> wrap HomeModel HomeMsg { model with history = model.subModel :: model.history }
        | TabsMsg(TabsScreen.ThreadsMsg(ThreadsScreen.ThreadSelected id)), _ ->
            MessagesScreen.init id
            |> wrap MessagesModel MessagesMsg
                { model with
                    history = model.subModel :: model.history
                    subHistory = (fun db -> MessagesScreen.sub id db |> MessagesMsg) :: model.subHistory }
        | PostMsg(PostScreen.OpenTag source), _ ->
            PostsComponent.init source |> wrap HomeModel HomeMsg { model with history = model.subModel :: model.history }
        | PostMsg subMsg, PostModel subModel -> PostScreen.update subModel subMsg |> wrap PostModel PostMsg model
        | MessagesMsg subMsg, MessagesModel subModel -> 
            MessagesScreen.update subModel subMsg |> wrap MessagesModel MessagesMsg model
        | TabsMsg subMsg, TabsModel subModel -> TabsScreen.update subModel subMsg |> wrap TabsModel TabsMsg model
        | HomeMsg subMsg, HomeModel subModel -> PostsComponent.update subModel subMsg |> wrap HomeModel HomeMsg model
        | LoginMsg(LoginScreen.ClosePage), LoginModel _ -> model, Cmd.ofMsg NavigateBack
        | LoginMsg subMsg, LoginModel subModel -> LoginScreen.update subModel subMsg |>  wrap LoginModel LoginMsg model
        | _ -> model, Cmd.none

    let view model dispatch =
        match model.subModel with
        | HomeModel subModel -> PostsComponent.view subModel (HomeMsg >> dispatch)
        | PostModel subModel -> PostScreen.view subModel (PostMsg >> dispatch)
        | ProfileModel subModel -> ProfileScreen.view subModel (ProfileMsg >> dispatch)
        | LoginModel subModel -> LoginScreen.view subModel (LoginMsg >> dispatch)
        | TagsModel subModel -> TagsScreen.view subModel (TagsMsg >> dispatch)
        | TabsModel subModel -> TabsScreen.view subModel (TabsMsg >> dispatch)
        | MessagesModel subModel -> MessagesScreen.view subModel (MessagesMsg >> dispatch)

    let setupBackHandler dispatch =
        let backHandler() =
            dispatch NavigateBack
            true
        setOnHardwareBackPressHandler backHandler

    let subscribe _ = Cmd.batch [ Cmd.ofSub setupBackHandler; sub ]

module InitSyncStore =
    open JoyReactor.Services
    open Browser.Blob
    open Fable.Core
    open Fable.Core.JsInterop
    module S = JoyReactor.SyncStore

    let init _ =

        S.fromJsonString := (fun json -> JS.JSON.parse json)

        S.sendToServer :=
            fun keyValues -> async {
                let form = FormData.Create()
                keyValues |> List.iter form.append                    
                
                let apiUrl = (sprintf "%s/sync" UrlBuilder.apiBaseUri)
                printfn "LOGX (2.1) | %O | %O" apiUrl (keyValues |> List.map fst)
                
                let! response =
                    fetch
                        apiUrl
                        [ Method HttpMethod.POST
                          Credentials RequestCredentials.Sameorigin
                          Body !^ form ]
                    |> Async.AwaitPromise
                
                let! respForm = response.formData() |> Async.AwaitPromise

                let parts : (string * obj) [] = respForm?_parts

                return
                    parts
                    |> Seq.map (fun (k, v) -> k, string v)
                    |> Seq.toList
            }

InitSyncStore.init()

Program.mkProgram App.init App.update App.view
|> Program.withSubscription App.subscribe
//|> Program.withConsoleTrace
|> Program.withReactNative "joyreact"
|> Program.run
