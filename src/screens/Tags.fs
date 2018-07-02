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
type Msg = TagsSynced of Result<Unit, Exception> | TagsLoaded of Tag []

let init = 
    let cmd =
        ReactiveStore.listenTagUpdates
        |> Cmd.ofSub
        |> Cmd.map TagsLoaded
    let cmd2 = ReactiveStore.syncTags |> flip Cmd.ofEffect TagsSynced
    { tags = [||]; loaded = false }, Cmd.batch [cmd; cmd2]

let update model = function
    | TagsLoaded tags -> 
        { model with tags = tags }, Cmd.none
    | TagsSynced (Ok _) -> 
        { model with loaded = true }, Cmd.none
    | TagsSynced (Error e) -> 
        raise e

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