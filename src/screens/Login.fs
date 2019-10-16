module LoginScreen

open Elmish
open Fable.ReactNative.Helpers
open Fable.ReactNative.Props
open JoyReactor

module S = Services
module UI = CommonUi

type Model =
    { username : string
      password : string
      isBusy : bool
      error : string option }

type Msg =
    | LoginMsg
    | LoginResultMsg of Result<unit, exn>
    | UsernameMsg of string
    | PasswordMsg of string
    | IgnoreMsg
    | ClosePage

module private Styles =
    let edit =
        TextInput.TextInputProperties.Style [ 
            BackgroundColor "white"
            TextStyle.Color "black"
            Padding $ 4.
            Height $ 45.
            FontSize 16.
            PaddingLeft $ 18.
            BorderColor "#ececec"
            BorderWidth 1.
            BorderRadius 4. ]

    let button margin =
        TouchableWithoutFeedbackProperties.Style [ MarginLeft margin
                                                   MarginRight margin
                                                   BackgroundColor UI.Colors.primary
                                                   BorderRadius 4.
                                                   Overflow ImageOverflow.Hidden ]

    let buttonText =
        TextProperties.Style [ FontWeight FontWeight.Bold
                               FontSize 13.
                               TextAlign TextAlignment.Center
                               Padding $ 15.
                               TextStyle.Color "white" ]

    let textInput text placeholder f ps =
        textInputWithChild 
            ([ edit
               TextInput.TextInputProperties.PlaceholderTextColor "gray"
               TextInput.TextInputProperties.Placeholder placeholder
               TextInput.TextInputProperties.OnChangeText f ] @ ps)
            text

let sub _ = IgnoreMsg

let init =
    { username = ""; password = ""; isBusy = false; error = None }, Cmd.none

let update model msg : Model * Cmd<Msg> =
    match msg with
    | LoginMsg ->
        { model with isBusy = true; error = None }, 
        S.login model.username model.password |> Cmd.ofEffect LoginResultMsg
    | LoginResultMsg(Ok _) -> { model with isBusy = false }, Cmd.ofMsg ClosePage
    | LoginResultMsg(Error e) -> { model with isBusy = false; error = Some <| string e }, Cmd.none
    | UsernameMsg x -> { model with username = x }, Cmd.none
    | PasswordMsg x -> { model with password = x }, Cmd.none
    | _ -> model, Cmd.none

let private viewButton dispatch title margin =
    touchableOpacity [ Styles.button margin; OnPress(dispatch <! LoginMsg) ] [ 
        text [ Styles.buttonText ] <| String.toUpper title ]

let view model dispatch =
    match model.isBusy with
    | true ->
        activityIndicator [ ViewProperties.Style [ Flex 1. ]
                            ActivityIndicator.Size Size.Large
                            ActivityIndicator.Color UI.Colors.orange ]
    | false ->
        view [ ViewProperties.Style [ Padding $ 20.; PaddingTop $ 50. ] ] [ 
            Styles.textInput model.username "Логин" (UsernameMsg >> dispatch)
                [ TextInput.AutoCapitalize AutoCapitalize.None ]
            view [ ViewProperties.Style [ Height $ 12. ] ] []
            Styles.textInput model.password "Пароль" (PasswordMsg >> dispatch) [ TextInput.SecureTextEntry false ]
            view [ ViewProperties.Style [ Height $ 12. ] ] []
            viewButton dispatch "Войти" $ 0.
            text [ TextProperties.Style [ TextStyle.Color "red"; Padding $ 10.; FontSize 20. ] ] 
                (model.error |> Option.defaultValue "") ]
