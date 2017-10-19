module ProfileScreen
open JoyReactor
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props

type Model = { profile : Profile option }

module private Styles =
    let rating = 
        TextProperties.Style [ AlignSelf Alignment.Center; Margin 20.; Color "#616161"; FontSize 20. ]
    let userName = 
        TextProperties.Style [ AlignSelf Alignment.Center; Margin 8.; Color "#616161"; FontSize 13. ]
    let avatar =
        ImageProperties.Style [ Height 0.; BorderTopLeftRadius 8.; BorderTopRightRadius 8. ]
    let progressToNewStar x =
        ViewProperties.Style [ Width x; BorderRadius 4.; Height 21.; BackgroundColor "#edc95b" ]
    let button margin =
        TouchableWithoutFeedbackProperties.Style [ MarginLeft margin; MarginRight margin; BackgroundColor "#e49421"; BorderRadius 4.; Overflow Overflow.Hidden ]
    let buttonText =
        TextProperties.Style [ FontWeight FontWeight.Bold; FontSize 13.; TextAlign TextAlignment.Center; Padding 15.; Color "white" ]

let private viewButton title margin =
    touchableOpacity [ Styles.button margin ] [
        text [ Styles.buttonText ] <| String.toUpper title ]

let private viewProfile (profile : Profile) =
    view [] [
        image [ Styles.avatar; Source [ Uri profile.userImage.url ] ]
        text [ Styles.userName ] profile.userName
        text [ Styles.rating ] (sprintf "Рейтинг: %f" profile.rating)
        view [ ViewProperties.Style [ Height 10. ] ] []
        view [ ViewProperties.Style [ Height 1.; BackgroundColor "#e4e4e4" ] ] []
        view [] [
            text [] (String.replicate profile.stars "*")
            text [] (String.replicate (max 0 (10 - profile.stars)) "*") ]
        view [ ViewProperties.Style [ Height 1.; BackgroundColor "#e4e4e4" ] ] []
        view [ ViewProperties.Style [ Padding 20.; BackgroundColor "white" ] ] [
            text [] "Прогресс до следующей звезды:"
            view [ ViewProperties.Style [ BorderRadius 4.; MarginTop 12.; Height 21.; BackgroundColor "#e4e4e4" ] ] [
                view [ Styles.progressToNewStar profile.progressToNewStar ] []
            ]
        ]
        view [ ViewProperties.Style [ Height 1.; BackgroundColor "#e4e4e4" ] ] []
        view [ ViewProperties.Style [ Height 10. ] ] []
        viewButton "Выйти" 20. ]

let view model =
    match model.profile with
    | Some p -> viewProfile p
    | None ->
        activityIndicator 
            [ ViewProperties.Style [ Flex 1. ]
              ActivityIndicator.Size Size.Large
              ActivityIndicator.Color "#ffb100" ]