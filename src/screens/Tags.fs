module TagsScreen

open Elmish
open JoyReactor
open JoyReactor.Types
module S = Services
module UI = CommonUi
type LocalDb = CofxStorage.LocalDb

type Model = { tags : Tag []; loaded : bool }

type Msg =
    | Refresh
    | RefreshComplete of Result<unit, exn>
    | TagsLoaded of Tag []
    | OpenPosts of Source

let sub (db : LocalDb) = [ db.topTags; db.userTags ] |> Array.concat |> TagsLoaded

let private syncTopTags =
    S.ApiRequests.downloadString UrlBuilder.home []
    >>= fun html -> SyncStore.dispatch (fun db -> { db with parseRequests = Set.union (Set.ofSeq [html]) db.parseRequests })

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
    | TagsLoaded tags -> { model with tags = addFavorite tags }, Cmd.none
    | Refresh ->
        { model with loaded = false },
        syncTopTags |> Cmd.ofEffect RefreshComplete
    | RefreshComplete _ -> { model with loaded = true }, Cmd.none
    | _ -> model, Cmd.none

open Fable.ReactNative.Helpers
open Fable.ReactNative.Props

module Styles =
    let image =
        ImageProperties.Style [ Width $ 48.; Height $ 48.; BorderRadius 24.; MarginRight $ 8. ]
    let label =
        TextProperties.Style [ FontSize 18.; TextStyle.Color "#404040"; AlignSelf Alignment.Center ]

let viewItem dispatch (tag : Tag) =
    touchableOpacity [ ActiveOpacity 0.4; OnPress(dispatch <! OpenPosts(tagToSource tag)) ] [
        view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding $ 8. ] ] [
            image [ Styles.image; Source <| remoteImage [ Uri tag.image ] ]
            text [ Styles.label ] tag.name ] ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] [
        UI.list model.tags (viewItem dispatch) (fun x -> x.name) [ OnRefresh(dispatch <! Refresh); Refreshing false ]
        UI.loadingView <| not model.loaded ]
