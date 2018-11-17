module ProfileScreen

open System
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNative
open Elmish
open JoyReactor
open JoyReactor.CommonUi
open JoyReactor.Types

module Cmd = JoyReactor.Free.Cmd
module Service = JoyReactor.Free.Service

let bind = JoyReactor.Free.bind

type Msg =
    | ProfileMsg of Result<Profile, Exception>
    | LoginMsg of LoginScreen.Msg
    | Logout

type ModelStage =
    | ProfileModel of Profile
    | LoadingModel
    | LoginModel

type Model =
    { stage: ModelStage
      subModel: LoginScreen.Model }

let init: Model * Cmd<Msg> =
    { stage = LoadingModel
      subModel = LoginScreen.init }, Cmd.ofEffect Service.loadMyProfile ProfileMsg

let update model =
    function 
    | Logout -> 
        { model with stage = LoadingModel }, 
        Cmd.ofEffect (Service.logout |> bind (fun _ -> Service.loadMyProfile)) ProfileMsg
    | ProfileMsg(Ok p) -> { model with stage = ProfileModel p }, Cmd.none
    | ProfileMsg _ -> 
        { model with stage = LoginModel
                     subModel = LoginScreen.init }, Cmd.none
    | LoginMsg subMsg -> 
        let loginModel, cmd = LoginScreen.update model.subModel subMsg
        { model with subModel = loginModel }, Cmd.map LoginMsg cmd

module private Styles =
    let rating =
        TextProperties.Style [ AlignSelf Alignment.Center
                               Margin 20.
                               TextStyle.Color "#616161"
                               FontSize 20. ]
    
    let userName =
        TextProperties.Style [ AlignSelf Alignment.Center
                               Margin 8.
                               TextStyle.Color "#616161"
                               FontSize 13. ]
    
    let avatar =
        ImageProperties.Style [ Width 90.
                                Height 90.
                                MarginTop 50.
                                AlignSelf Alignment.Center
                                BorderRadius 45. ]
    
    let progressToNewStar x =
        ViewProperties.Style [ Width x
                               BorderRadius 4.
                               Height 21.
                               BackgroundColor "#edc95b" ]
    
    let button margin =
        TouchableWithoutFeedbackProperties.Style [ MarginLeft margin
                                                   MarginRight margin
                                                   BackgroundColor Colors.primaryColor
                                                   BorderRadius 4.
                                                   Overflow Overflow.Hidden ]
    
    let buttonText =
        TextProperties.Style [ FontWeight FontWeight.Bold
                               FontSize 13.
                               TextAlign TextAlignment.Center
                               Padding 15.
                               TextStyle.Color "white" ]
    
    let starsPanel =
        ViewProperties.Style [ BackgroundColor "white"
                               Height 50.
                               FlexDirection FlexDirection.Row
                               AlignItems ItemAlignment.Center
                               JustifyContent JustifyContent.Center ]
    
    let star color =
        TextProperties.Style [ FontSize 25.
                               TextStyle.Color color ]
    
    let divider =
        view [ ViewProperties.Style [ Height 1.
                                      BackgroundColor Colors.gray ] ] []

let private viewButton title margin onPress =
    touchableOpacity [ Styles.button margin
                       OnPress onPress ] [ text [ Styles.buttonText ] <| String.toUpper title ]

let private viewProfile (profile: Profile) dispatch =
    view [] [ image [ Styles.avatar
                      Source [ Uri profile.userImage.url ] ]
              text [ Styles.userName ] profile.userName
              text [ Styles.rating ] (sprintf "Рейтинг: %g" profile.rating)
              view [ ViewProperties.Style [ Height 10. ] ] []
              Styles.divider
              
              view [ Styles.starsPanel ] 
                  [ text [ Styles.star "#edc95b" ] (String.replicate profile.stars "★")
                    text [ Styles.star "#e4e6e7" ] (String.replicate (max 0 (10 - profile.stars)) "★") ]
              Styles.divider

              view [ ViewProperties.Style [ Padding 20.
                                            BackgroundColor "white" ] ] 
                  [ text [] "Прогресс до следующей звезды:"
                    
                    view [ ViewProperties.Style [ BorderRadius 4.
                                                  MarginTop 12.
                                                  Height 21.
                                                  BackgroundColor Colors.gray ] ] 
                        [ view [ Styles.progressToNewStar profile.progressToNewStar ] [] ] ]
              Styles.divider
              view [ ViewProperties.Style [ Height 10. ] ] []
              viewButton "Выйти" 20. (dispatch <! Logout) ]

let view model dispatch =
    let content =
        match model.stage with
        | LoadingModel -> 
            activityIndicator [ ViewProperties.Style [ Flex 1. ]
                                ActivityIndicator.Size Size.Large
                                ActivityIndicator.Color "#ffb100" ]
        | LoginModel -> LoginScreen.view model.subModel (LoginMsg >> dispatch)
        | ProfileModel p -> viewProfile p dispatch
    view [ ViewProperties.Style [ BackgroundColor "#fafafa"
                                  Flex 1. ] ] [ content ]
