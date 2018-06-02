module LoginScreen

open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNative
open Elmish
open JoyReactor
module S = JoyReactor.Service

type Model = { username: string; password: string; isBusy: bool; error: string option }
type Msg = LoginMsg | LoginResultMsg of Result<Unit, string> | UsernameMsg of string | PasswordMsg of string

module private Styles =
    let edit = 
        TextInput.Style [ BackgroundColor "white"; TextStyle.Color "black"; Padding 4.; Height 45.; FontSize 16.; PaddingLeft 18.; BorderColor "#ececec"; BorderWidth 1.; BorderRadius 4. ]
    let button margin =
        TouchableWithoutFeedbackProperties.Style [ MarginLeft margin; MarginRight margin; BackgroundColor "#e49421"; BorderRadius 4.; Overflow Overflow.Hidden ]
    let buttonText =
        TextProperties.Style [ FontWeight FontWeight.Bold; FontSize 13.; TextAlign TextAlignment.Center; Padding 15.; TextStyle.Color "white" ]

let init = { username = ""; password = ""; isBusy = false; error = None }, Cmd<Msg>.none

let update model msg: Model * Cmd<Msg> =
    match msg with
    | LoginMsg -> 
        { model with isBusy = true; error = None }, 
        Cmd.ofPromise (S.login model.username model.password) LoginResultMsg
    | LoginResultMsg (Ok _) -> { model with isBusy = false }, Cmd.none
    | LoginResultMsg (Error e) -> { model with isBusy = false; error = Some e }, Cmd.none
    | UsernameMsg x -> { model with username = x }, Cmd.none
    | PasswordMsg x -> { model with password = x }, Cmd.none

let private viewButton dispatch title margin =
    touchableOpacity 
        [ Styles.button margin 
          OnPress (fun _ -> LoginMsg |> dispatch) ] 
        [ text [ Styles.buttonText ] <| String.toUpper title ]

let view model dispatch =
    match model.isBusy with
    | true -> 
        activityIndicator [ ActivityIndicator.Style [ Flex 1. ]
                            ActivityIndicator.Size Size.Large
                            ActivityIndicator.Color "#ffb100" ]
    | false ->
        view [ ViewProperties.Style [ Padding 20.; PaddingTop 50. ] ] 
             [ textInput [ Styles.edit
                           TextInput.PlaceholderTextColor "gray"
                           TextInput.Placeholder "Логин" 
                           TextInput.OnChangeText (UsernameMsg >> dispatch) ] 
                         model.username
               view [ ViewProperties.Style [ Height 12. ] ] []
               textInput [ Styles.edit
                           TextInput.PlaceholderTextColor "gray"
                           TextInput.Placeholder "Пароль" 
                           TextInput.SecureTextEntry true
                           TextInput.OnChangeText (PasswordMsg >> dispatch) ] 
                         model.password
               view [ ViewProperties.Style [ Height 12. ] ] []
               viewButton dispatch "Войти" 0.
               text [ TextProperties.Style [ TextStyle.Color "red"; Padding 10.; FontSize 20. ] ] 
                    (model.error |> Option.defaultValue "") ]