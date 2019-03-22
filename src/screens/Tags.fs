module TagsScreen

open System
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNative
open Elmish
open JoyReactor
open JoyReactor.Types

module UI = JoyReactor.CommonUi
module Cmd = JoyReactor.Free.Cmd
module Service = JoyReactor.Free.Service

type Model = { tags: Tag []; loaded: Boolean }

type Msg =
    | FromCache of Result<Tag [], Exception>
    | FromWeb of Result<Tag [], Exception>
    | TagsLoaded of Tag []
    | OpenPosts of Source
    | Refresh

let init =
    let cmd =
        Cmd.batch [ Service.getTagsFromCache |> flip Cmd.ofEffect FromCache
                    Service.getTagsFromWeb |> flip Cmd.ofEffect FromWeb ]
    { tags = [||]
      loaded = false }, cmd

let addFavorite tags =
    Array.concat [ [| { name = "Избранное"
                        image = "http://img1." + UrlBuilder.domain + "/pics/avatar/tag/1279" } |]
                   tags ]

let tagToSourse tag =
    match tag.name with
    | "Избранное" -> FavoriteSource
    | _ -> TagSource tag.name

let update model =
    function
    | Refresh -> { model with loaded = false }, Service.getTagsFromWeb |> flip Cmd.ofEffect FromWeb
    | FromCache(Ok tags) -> { model with tags = addFavorite tags }, Cmd.none
    | FromWeb(Ok tags) -> { model with tags = addFavorite tags; loaded = true }, Cmd.none
    | FromCache(Error e) -> raise e
    | FromWeb(Error e) -> raise e
    | _ -> model, Cmd.none

module Styles =
    let image =
        ImageProperties.Style [ Width 48.
                                Height 48.
                                BorderRadius 24.
                                MarginRight 8. ]

    let label =
        TextProperties.Style [ FontSize 18.
                               TextStyle.Color "#404040"
                               AlignSelf Alignment.Center ]

let viewItem dispatch (x: Tag) =
    touchableOpacity [ ActiveOpacity 0.4; OnPress(dispatch <! OpenPosts(tagToSourse x)) ] [
        view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding 8. ] ] [
            image [ Styles.image; Source [ Uri x.image ] ]
            text [ Styles.label ] x.name ] ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] [
        UI.list model.tags (viewItem dispatch) (fun x -> x.name) [
            FlatListProperties.OnRefresh (Func<_, _>(fun _ -> dispatch Refresh))
            FlatListProperties.Refreshing false ]
        UI.loadingView <| not model.loaded ]
