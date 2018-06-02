module TagsScreen

open System
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNative
open Elmish
open JoyReactor.Utils
open JoyReactor
open JoyReactor.Types

type Model = { dataSource: ListViewDataSource<Tag>; loaded: Boolean }
type Msg = TagsLoaded of Result<Tag list, string>

let init = { dataSource = emptyDataSource(); loaded = false }, 
           Service.loadTags "_y2k" |> flip Cmd.ofPromise_ TagsLoaded

let update model msg =
    match msg with
    | TagsLoaded (Ok tags) -> 
        tags
        |> List.toArray 
        |> flip updateDataSource model.dataSource
        |> fun x -> { model with dataSource = x; loaded = true }, Cmd.none
    | TagsLoaded (Error e) -> failwith e

module Styles =
    let image = 
        ImageProperties.Style [ Width 48.; Height 48.; BorderRadius 24.; MarginRight 8. ]
    let label =
        TextProperties.Style [ FontSize 18.; TextStyle.Color "#404040"; AlignSelf Alignment.Center ]

let viewItem (x: Tag) = 
    touchableOpacity [ ActiveOpacity 0.4 ] [
        view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding 8. ] ] [
            image [ Styles.image; Source [ Uri x.image ] ]
            text [ Styles.label ] x.name ] ]

let view model =
    let content =
        match model.loaded with
        | true ->
            listView model.dataSource [
                ListViewProperties.RenderRow(Func<_,_,_,_,_>(fun (i: Tag) _ _ _ -> viewItem i)) ]
        | false ->
            activityIndicator [ 
                ViewProperties.Style [ Flex 1. ]
                ActivityIndicator.Size Size.Large
                ActivityIndicator.Color "#ffb100" ]
    view [ ViewProperties.Style [ BackgroundColor "#fafafa"; Flex 1. ] ] 
         [ content ]