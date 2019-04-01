module TagsScreen

open Elmish
open JoyReactor
open JoyReactor.Types

module S = JoyReactor.Services
module UI = JoyReactor.CommonUi
module Cmd = JoyReactor.Services.Cmd

type Model = { tags : Tag []; loaded : bool }

type Msg =
    | FromCache of Result<Tag [], exn>
    | FromWeb of Result<Tag [], exn>
    | TagsLoaded of Tag []
    | OpenPosts of Source
    | Refresh

let init =
    { tags = [||]; loaded = false },
    Cmd.batch [ S.getTagsFromCache |> Cmd.ofEff FromCache
                S.getTagsFromWeb |> Cmd.ofEff FromWeb ]

let addFavorite tags =
    Array.concat [
        [| { name = "Избранное"; image = "http://img1." + UrlBuilder.domain + "/pics/avatar/tag/1279" } |]
        tags ]

let tagToSourse tag =
    match tag.name with
    | "Избранное" -> FavoriteSource
    | _ -> TagSource tag.name

let update model =
    function
    | Refresh -> { model with loaded = false }, S.getTagsFromWeb |> Cmd.ofEff FromWeb
    | FromCache(Ok tags) -> { model with tags = addFavorite tags }, Cmd.none
    | FromWeb(Ok tags) ->
        { model with tags = addFavorite tags; loaded = true },
        S.saveTagToCache tags |> Cmd.ofEff0
    | FromCache(Error e) -> raise e
    | FromWeb(Error e) -> raise e
    | _ -> model, Cmd.none

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props

module Styles =
    let image =
        ImageProperties.Style [ Width $ 48.; Height $ 48.; BorderRadius 24.; MarginRight $ 8. ]
    let label =
        TextProperties.Style [ FontSize 18.
                               TextStyle.Color "#404040"
                               AlignSelf Alignment.Center ]

let viewItem dispatch (x : Tag) =
    touchableOpacity [ ActiveOpacity 0.4; OnPress(dispatch <! OpenPosts(tagToSourse x)) ] [
        view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding $ 8. ] ] [
            image [ Styles.image; Source <| remoteImage [ Uri x.image ] ]
            text [ Styles.label ] x.name ] ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] [
        UI.list model.tags (viewItem dispatch) (fun x -> x.name) [
            FlatListProperties.OnRefresh(dispatch <! Refresh)
            FlatListProperties.Refreshing false ]
        UI.loadingView <| not model.loaded ]
