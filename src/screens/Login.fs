module LoginScreen

open Elmish
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open JoyReactor

module Cmd = JoyReactor.Services.Cmd
module S = JoyReactor.Services

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
                                                   BackgroundColor "#e49421"
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

let init =
    { username = ""
      password = ""
      isBusy = false
      error = None }

let update model msg : Model * Cmd<Msg> =
    match msg with
    | LoginMsg ->
        { model with isBusy = true; error = None }, 
        S.login model.username model.password |> Cmd.ofEff LoginResultMsg
    | LoginResultMsg(Ok _) -> { model with isBusy = false }, Cmd.none
    | LoginResultMsg(Error e) ->
        { model with isBusy = false; error = Some <| string e }, 
        Cmd.none
    | UsernameMsg x -> { model with username = x }, Cmd.none
    | PasswordMsg x -> { model with password = x }, Cmd.none

let private viewButton dispatch title margin =
    touchableOpacity [ Styles.button margin; OnPress(dispatch <! LoginMsg) ] [ 
        text [ Styles.buttonText ] <| String.toUpper title ]

let view model dispatch =
    match model.isBusy with
    | true ->
        activityIndicator [ ViewProperties.Style [ Flex 1. ]
                            ActivityIndicator.Size Size.Large
                            ActivityIndicator.Color "#ffb100" ]
    | false ->
        view [ ViewProperties.Style [ Padding $ 20.; PaddingTop $ 50. ] ] [ 
            Styles.textInput model.username "Логин" (UsernameMsg >> dispatch)
                [ TextInput.AutoCapitalize AutoCapitalize.None ]
            view [ ViewProperties.Style [ Height $ 12. ] ] []
            Styles.textInput model.password "Пароль" (PasswordMsg >> dispatch) [ TextInput.SecureTextEntry true ]
            view [ ViewProperties.Style [ Height $ 12. ] ] []
            viewButton dispatch "Войти" $ 0.
            text [ TextProperties.Style [ TextStyle.Color "red"; Padding $ 10.; FontSize 20. ] ] 
                (model.error |> Option.defaultValue "") ]
