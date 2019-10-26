module TagsScreen

open Elmish
open JoyReactor
open JoyReactor.Types
module UI = CommonUi
module R = JoyReactor.Services.EffRuntime
module D = JoyReactor.SyncDomain
type LocalDb = CofxStorage.LocalDb

type Model =
    { tags : Tag []; loaded : bool }
    with static member empty = { tags = [||]; loaded = false }

type Msg =
    | Refresh
    | RefreshComplete of Result<unit, exn>
    | TagsChanged of Tag []
    | OpenPosts of Source

let sub (db : LocalDb) = 
    [ db.topTags |> Map.toSeq |> Seq.map snd |> Seq.toArray
      db.userTags  |> Map.toSeq |> Seq.map snd |> Seq.toArray ] 
    |> Array.concat |> TagsChanged

let init = { tags = [||]; loaded = false }, Cmd.ofMsg Refresh

let addFavorite tags =
    Array.concat [
        [| { name = "Избранное"; image = sprintf "http://img1.%s/pics/avatar/tag/1279" UrlBuilder.domain } |]
        tags ]

let tagToSource tag =
    match tag.name with
    | "Избранное" -> FavoriteSource
    | _ -> TagSource tag.name

let update (model : Model) = function
    | TagsChanged tags -> { model with tags = addFavorite tags }, Cmd.none
    | Refresh -> 
        { model with loaded = false }, 
        Cmd.batch [
            D.topTags |> R.run |> Cmd.map RefreshComplete
            D.userTags |> R.run |> Cmd.map RefreshComplete ]
    | RefreshComplete _ -> { model with loaded = true }, Cmd.none
    | _ -> model, Cmd.none

open Fable.ReactNative.Helpers
open Fable.ReactNative.Props

module Styles =
    let image () =
        ImageProperties.Style [ Width $ 48.; Height $ 48.; BorderRadius 24.; MarginRight $ 8. ]
    let label () =
        TextProperties.Style [ FontSize 18.; TextStyle.Color "#404040"; AlignSelf Alignment.Center ]

let viewItem dispatch (tag : Tag) =
    touchableOpacity [ ActiveOpacity 0.4; OnPress(dispatch <! OpenPosts(tagToSource tag)) ] [
        view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding $ 8. ] ] [
            image [ Styles.image (); Source <| remoteImage [ Uri tag.image ] ]
            text [ Styles.label () ] tag.name ] ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] [
        UI.list model.tags (viewItem dispatch) (fun x -> x.name) [ OnRefresh(dispatch <! Refresh); Refreshing false ]
        UI.loadingView <| not model.loaded ]
