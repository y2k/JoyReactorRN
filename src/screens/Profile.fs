module ProfileScreen
open JoyReactor
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props

type Model = { profile : Profile option }

module private Styles =
    let rating = TextProperties.Style []
    let userName = TextProperties.Style []
    let avatar =
        ImageProperties.Style [ Height 0.; BorderTopLeftRadius 8.; BorderTopRightRadius 8. ]

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
            view [] []
        ]
        ]

let view model =
    match model.profile with
    | Some p -> viewProfile p
    | None ->
        activityIndicator 
            [ ViewProperties.Style [ Flex 1. ]
              ActivityIndicator.Size Size.Large
              ActivityIndicator.Color "#ffb100" ]