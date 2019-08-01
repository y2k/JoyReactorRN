module ProfileScreen

open Elmish
open JoyReactor
open JoyReactor.CommonUi
open JoyReactor.Types
module UI = JoyReactor.CommonUi
module Cmd = JoyReactor.Services.Cmd
module S = JoyReactor.Services
type LocalDb = JoyReactor.CofxStorage.LocalDb

type Msg =
    | ProfileMsg of Profile option
    | LoginMsg of LoginScreen.Msg
    | Logout
    | LogoutComplete of Result<unit, exn>

type ModelStage =
    | ProfileModel of Profile
    | LoadingModel
    | LoginModel

type Model = { stage : ModelStage; subModel : LoginScreen.Model }

let sub (db : LocalDb) = ProfileMsg db.profile

let init : Model * Cmd<Msg> =
    { stage = LoadingModel; subModel = LoginScreen.init },
    S.syncMyProfile |> Cmd.ofEffect0

let update model = function
    | ProfileMsg (Some p) -> 
        { model with stage = ProfileModel p }, Cmd.none
    | ProfileMsg None -> 
        { model with stage = LoginModel; subModel = LoginScreen.init }, Cmd.none
    | Logout ->
        { model with stage = LoadingModel }, S.logout |> Cmd.ofEffect LogoutComplete
    | LogoutComplete _ -> model, S.syncMyProfile |> Cmd.ofEffect0
    | LoginMsg subMsg ->
        let loginModel, cmd = LoginScreen.update model.subModel subMsg
        { model with subModel = loginModel }, Cmd.map LoginMsg cmd

open Fable.ReactNative.Helpers
open Fable.ReactNative.Props

module private Styles =
    let rating =
        TextProperties.Style [ AlignSelf Alignment.Center
                               Margin $ 20.
                               TextStyle.Color UI.Colors.darkGray
                               FontSize 20. ]

    let userName =
        TextProperties.Style [ AlignSelf Alignment.Center
                               Margin $ 8.
                               TextStyle.Color UI.Colors.darkGray
                               FontSize 13. ]

    let avatar =
        ImageProperties.Style [ Width $ 90.
                                Height $ 90.
                                MarginTop $ 50.
                                AlignSelf Alignment.Center
                                BorderRadius 45. ]

    let progressToNewStar x =
        ViewProperties.Style [ Width x
                               BorderRadius 4.
                               Height $ 21.
                               BackgroundColor "#edc95b" ]

    let button margin =
        TouchableWithoutFeedbackProperties.Style [ MarginLeft margin
                                                   MarginRight margin
                                                   BackgroundColor Colors.primary
                                                   BorderRadius 4.
                                                   Overflow ImageOverflow.Hidden ]

    let buttonText =
        TextProperties.Style [ FontWeight FontWeight.Bold
                               FontSize 13.
                               TextAlign TextAlignment.Center
                               Padding $ 15.
                               TextStyle.Color "white" ]

    let starsPanel =
        ViewProperties.Style [ BackgroundColor "white"
                               Height $ 50.
                               FlexDirection FlexDirection.Row
                               AlignItems ItemAlignment.Center
                               JustifyContent JustifyContent.Center ]

    let star color =
        TextProperties.Style [ FontSize 25.
                               TextStyle.Color color ]

    let divider =
        view [ ViewProperties.Style [ Height $ 1.
                                      BackgroundColor Colors.gray ] ] []

let private viewButton title margin onPress =
    touchableOpacity [ Styles.button margin; OnPress onPress ] [
        text [ Styles.buttonText ] <| String.toUpper title ]

let private viewProfile (profile : Profile) dispatch =
    view [] [
        image [ Styles.avatar; Source <| remoteImage [ Uri profile.userImage.url ] ]
        text [ Styles.userName ] profile.userName
        text [ Styles.rating ] (sprintf "Рейтинг: %g" profile.rating)
        view [ ViewProperties.Style [ Height $ 10. ] ] []
        Styles.divider
        view [ Styles.starsPanel ] [
            text [ Styles.star "#edc95b" ] (String.replicate profile.stars "★")
            text [ Styles.star "#e4e6e7" ] (String.replicate (max 0 (10 - profile.stars)) "★") ]
        Styles.divider
        view [ ViewProperties.Style [ Padding $ 20.; BackgroundColor "white" ] ] [
            text [] "Прогресс до следующей звезды:"
            view [ ViewProperties.Style [ BorderRadius 4.; MarginTop $ 12.; Height $ 21.; BackgroundColor Colors.gray ] ] [
                view [ Styles.progressToNewStar $ profile.progressToNewStar ] [] ] ]
        Styles.divider
        view [ ViewProperties.Style [ Height $ 10. ] ] []
        viewButton "Выйти" (dip 20.) (dispatch <! Logout) ]

let view model dispatch =
    let content =
        match model.stage with
        | LoadingModel ->
            activityIndicator [ ViewProperties.Style [ Flex 1. ]
                                ActivityIndicator.Size Size.Large
                                ActivityIndicator.Color UI.Colors.orange ]
        | LoginModel -> LoginScreen.view model.subModel (LoginMsg >> dispatch)
        | ProfileModel p -> viewProfile p dispatch
    view [ ViewProperties.Style [ BackgroundColor "#fafafa"; Flex 1. ] ] [
        content ]
