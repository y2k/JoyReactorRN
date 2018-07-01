module TagsScreen

open System
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNative
open Elmish

open JoyReactor
open JoyReactor.Types
open JoyReactor.Utils
open JoyReactor.CommonUi

type Model = { tags: Tag []; loaded: Boolean }
type Msg = TagsLoaded of Result<Tag list, string>

let init = 
    { tags = [||]; loaded = false }, 
    Service.loadMyTags |> flip Cmd.ofEffect TagsLoaded

let update model msg =
    match msg with
    | TagsLoaded (Ok tags) -> 
        { model with tags = List.toArray tags; loaded = true }, Cmd.none
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
    view [ ViewProperties.Style [ Flex 1. ] ] 
         [ myFlatList model.tags viewItem (fun x -> x.name) []
           statusView <| (if model.loaded then Some <| Ok () else None) ]