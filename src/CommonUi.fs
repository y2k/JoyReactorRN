module JoyReactor.CommonUi

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open System

let primaryColor = "#e49421"

module Colors =
    let primaryColor = "#e49421"
    let gray = "#e4e4e4"

module private Styles =
    let tabButtonOuter selected =
        TouchableWithoutFeedbackProperties.Style [ Flex 1.
                                                   Margin $ 4.
                                                   BackgroundColor(if selected then "#d48411"
                                                                   else "#e49421")
                                                   BorderRadius 4.
                                                   Overflow ImageOverflow.Hidden ]

    let tabButtonInner =
        TextProperties.Style [ FontWeight FontWeight.Bold
                               FontSize 13.
                               TextAlign TextAlignment.Center
                               Padding $ 15.
                               TextStyle.Color "white" ]

let button title f =
    let nextButtonOutter =
        TouchableWithoutFeedbackProperties.Style [ Margin $ 4.
                                                   BackgroundColor "#e49421"
                                                   BorderRadius 4.
                                                   Height $ 48.
                                                   Overflow ImageOverflow.Hidden ]

    let tabButtonInner =
        TextProperties.Style [ FontWeight FontWeight.Bold
                               FontSize 13.
                               TextAlign TextAlignment.Center
                               Padding $ 15.
                               TextStyle.Color "white" ]

    touchableOpacity [ nextButtonOutter; OnPress f ] [
        text [ tabButtonInner ] title ]

let inline list (items : 'a []) f fid props =
    flatList items ([ FlatListProperties.KeyExtractor(fun (i : 'a) _ -> fid i)
                      FlatListProperties.RenderItem(fun (i : FlatListRenderItemInfo<'a>) -> f i.item) ]
                    @ props)

let indicatorView =
    activityIndicator [ ViewProperties.Style [ Flex 1. ]
                        ActivityIndicator.Size Size.Large
                        ActivityIndicator.Color "#ffb100" ]

let viewNavigationBar selected onSelect =
    let button title index =
        touchableOpacity [ Styles.tabButtonOuter (selected = index)
                           OnPress(fun _ -> onSelect index) ] [ text [ Styles.tabButtonInner ] title ]
    view [ ViewProperties.Style [ FlexDirection FlexDirection.Row ] ] [ button "Home" 0
                                                                        button "Tags" 1
                                                                        button "Messages" 2
                                                                        button "Profile" 3 ]

let statusView status =
    match status with
    | Some(Ok _) -> view [] []
    | Some(Error _) -> text [] "ERROR"
    | None ->
        activityIndicator [ ViewProperties.Style [ BackgroundColor "#212121"
                                                   Padding $ 4. ]
                            ActivityIndicator.Size Size.Large
                            ActivityIndicator.Color "#ffb100" ]

let loadingView inProgress =
    if inProgress then
        text [ TextProperties.Style [ BackgroundColor primaryColor
                                      Padding $ 10.
                                      TextStyle.Color "white"
                                      TextStyle.FontSize 18. ] ] "Loading"
    else view [] []

let reloadButton show title dispatch =
    if show
        then view [] []
        else
            touchableOpacity [ TouchableWithoutFeedbackProperties.Style [
                                   BackgroundColor primaryColor; Overflow ImageOverflow.Hidden ]
                               OnPress dispatch ] [
                text [ TextProperties.Style [ Padding $ 10.; TextStyle.Color "white"; TextStyle.FontSize 18. ] ]
                    title ]

let roundButton title dispatch props =
    let nextButtonOutter =
        TouchableWithoutFeedbackProperties.Style([ Margin $ 2.
                                                   BackgroundColor primaryColor
                                                   BorderRadius 4.
                                                   Height $ 48.
                                                   JustifyContent JustifyContent.Center
                                                   Overflow ImageOverflow.Hidden ]
                                                 @ props)

    let tabButtonInner : ITextProperties list =
        [ TextProperties.NumberOfLines 1.
          TextProperties.Style [ FontWeight FontWeight.Bold
                                 FontSize 14.
                                 TextAlign TextAlignment.Center
                                 TextStyle.Color "white" ] ]

    touchableOpacity [ nextButtonOutter
                       OnPress dispatch ] [ text tabButtonInner title ]

let iconView =
    text [ TextProperties.Style [ FontFamily "icomoon"; TextStyle.Color "#ffb100" ] ]
        "\ue8b5"
