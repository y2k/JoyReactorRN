module LoginScreen

open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNative
open Elmish
open JoyReactor

type Model = Model
type Msg = Msg

module private Styles =
    let edit = 
        TextInput.Style [ BackgroundColor "white"; Color "black"; Padding 4.; Height 45.; FontSize 16.; PaddingLeft 18.; BorderColor "#ececec"; BorderWidth 1.; BorderRadius 4. ]
    let button margin =
        TouchableWithoutFeedbackProperties.Style [ MarginLeft margin; MarginRight margin; BackgroundColor "#e49421"; BorderRadius 4.; Overflow Overflow.Hidden ]
    let buttonText =
        TextProperties.Style [ FontWeight FontWeight.Bold; FontSize 13.; TextAlign TextAlignment.Center; Padding 15.; Color "white" ]

let init = Model, Cmd<Msg>.none

let private viewButton title margin =
    touchableOpacity [ Styles.button margin ] [
        text [ Styles.buttonText ] <| String.toUpper title ]

let view _ =
    view [ ViewProperties.Style [ Padding 20.; PaddingTop 50. ] ] [
        textInput [ Styles.edit; TextInput.PlaceholderTextColor "gray"; TextInput.Placeholder "Логин" ] ""
        view [ ViewProperties.Style [ Height 12. ] ] []
        textInput [ Styles.edit; TextInput.PlaceholderTextColor "gray"; TextInput.Placeholder "Пароль" ] ""
        view [ ViewProperties.Style [ Height 12. ] ] []
        viewButton "Войти" 0.
    ]