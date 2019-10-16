module ProfileScreen

open Elmish
open JoyReactor
open JoyReactor.CommonUi
open JoyReactor.Types
module UI = CommonUi
module S = Services
type LocalDb = CofxStorage.LocalDb

type Msg =
    | ProfileChanged of Profile option
    | SyncComplete of Result<unit, exn>
    | Logout
    | LogoutComplete of Result<unit, exn>
    | Login

type Model = { profile : Profile option; userName : string option }

let sub (db : LocalDb) = ProfileChanged db.profile

let init : Model * Cmd<Msg> =
    { profile = None; userName = None },
    (S.runSyncEffect SyncDomain.syncMyProfile) |> Cmd.ofEffect SyncComplete

let update model = function
    | ProfileChanged p -> { model with profile = p }, Cmd.none
    | SyncComplete _ -> model, Cmd.none
    | Logout -> { profile = None; userName = None }, S.logout |> Cmd.ofEffect LogoutComplete
    | LogoutComplete _ -> model, (S.runSyncEffect SyncDomain.syncMyProfile) |> Cmd.ofEffect0
    | _ -> model, Cmd.none

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
        match model.profile with
        | Some p -> viewProfile p dispatch
        | None ->
            view [ ViewProperties.Style [ Flex 1.; JustifyContent JustifyContent.Center ] ] [
                viewButton "Login" (dip 20.) (dispatch <! Login) ]
    view [ ViewProperties.Style [ BackgroundColor "#fafafa"; Flex 1. ] ] [
        content ]
