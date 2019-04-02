module Home

open Elmish
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open JoyReactor
open JoyReactor.Types
open JoyReactor.Utils

module UI = JoyReactor.CommonUi
module S = JoyReactor.Services.Posts
module Cmd = JoyReactor.Services.Cmd

type PostState = | Actual of Post | Divider | Old of Post

type Msg =
    | PostsLoadedFromCache of Result<PostsWithLevels, exn>
    | PostsLoaded of Result<PostsWithLevels, exn>
    | LoadNextPage
    | OpenPost of Post
    | Refresh
    | ApplyUpdate

type Model =
    { syncState : PostsWithLevels
      items : PostState []
      status : Result<unit, exn> option
      source : Source }

let init source =
    { syncState = PostsWithLevels.empty; items = [||]; status = None; source = source },
    S.getCachedPosts source |> Cmd.ofEff PostsLoadedFromCache

let postsToPostStates posts =
    if Array.isEmpty posts.old && Array.isEmpty posts.actual
        then [||]
        else Array.concat [
                 posts.actual |> Array.map Actual
                 (if (not <| Array.isEmpty posts.preloaded) || Array.isEmpty posts.actual
                     then [||]
                     else [| Divider |])
                 posts.old |> Array.map Old ]

let update model msg : Model * Cmd<Msg> =
    match msg with
    | PostsLoadedFromCache(Ok x) ->
        { model with items = postsToPostStates x
                     syncState = x
                     status = if Array.isEmpty x.actual then None else Some <| Ok() },
        S.syncFirstPage model.source model.syncState |> Cmd.ofEff PostsLoaded
    | PostsLoaded(Ok x) ->
        { model with items = postsToPostStates x
                     syncState = x
                     status = if Array.isEmpty x.actual then None else Some <| Ok() },
        S.savePostsToCache model.source model.syncState |> Cmd.ofEff0
    | PostsLoaded(Error e) -> log e { model with status = Some <| Error e }, Cmd.none
    | ApplyUpdate ->
        model, S.applyUpdate model.source model.syncState |> Cmd.ofEff PostsLoaded
    | Refresh ->
        if Array.isEmpty model.syncState.preloaded
            then
                { model with status = None },
                S.syncFirstPage model.source PostsWithLevels.empty |> Cmd.ofEff PostsLoaded
            else model, Cmd.ofMsg ApplyUpdate
    | LoadNextPage ->
        { model with status = None },
        S.syncNextPage model.source model.syncState |> Cmd.ofEff PostsLoaded
    | _ -> model, Cmd.none

module private Styles =
    let nextButtonOutter enabled =
        TouchableWithoutFeedbackProperties.Style [ Margin $ 4.
                                                   BackgroundColor(if enabled then UI.Colors.primary else "#e4942100")
                                                   BorderRadius 4.
                                                   Overflow ImageOverflow.Hidden ]

    let nextButtonInner =
        TextProperties.Style [ FontWeight FontWeight.Bold
                               FontSize 13.
                               TextAlign TextAlignment.Center
                               Padding $ 15.
                               TextStyle.Color "white" ]

    let card =
        ViewProperties.Style [ AlignItems ItemAlignment.Stretch
                               BackgroundColor "white"
                               BorderColor "#eee"
                               BorderWidth 1.
                               BorderRadius 8.
                               Overflow ImageOverflow.Hidden ]

    let avatar =
        ImageProperties.Style [ Width $ 36.
                                Height $ 36.
                                BorderRadius 18.
                                MarginRight $ 9. ]

    let userName =
        TextProperties.Style [ FontWeight FontWeight.Bold
                               FontSize 14.
                               TextStyle.Color UI.Colors.darkGray ]

let viewNextButton dispatch isSyncing =
    let onPress = if isSyncing then ignore else dispatch <! LoadNextPage
    touchableOpacity [ Styles.nextButtonOutter <| not isSyncing; OnPress onPress ] [
        text [ Styles.nextButtonInner ] "Load next page" ]

let viewPostImage post =
    post.image
    |> Option.map (Image.urlWithHeight (Globals.Dimensions.get("screen").width))
    |> function
    | Some(img, h) ->
        image [ ImageProperties.Style [ Height $ h; BorderTopLeftRadius 8.; BorderTopRightRadius 8. ]
                Source <| remoteImage [ Uri img ] ]
    | None -> view [] []

let iconView =
    text [ TextProperties.Style [ FontFamily "icomoon"; TextStyle.Color UI.Colors.orange ] ]
        "\ue8b5"

let viewItem post dispatch =
    touchableHighlight [ TouchableHighlightProperties.Style [ Margin $ 4. ]
                         TouchableHighlightProperties.ActiveOpacity 0.7
                         OnPress(always (OpenPost post) >> dispatch) ] [
        view [ Styles.card ] [
            viewPostImage post
            view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Margin $ 9. ] ] [
                image [ Styles.avatar; Source <| remoteImage [ Uri post.userImage.url ] ]
                view [ ViewProperties.Style [ Flex 1. ] ] [
                    text [ Styles.userName ] post.userName
                    view [ ViewProperties.Style [ AlignSelf Alignment.FlexEnd; FlexDirection FlexDirection.Row ] ] [
                        UI.iconView
                        text [ TextProperties.Style [ MarginLeft $ 8.; TextStyle.Color "#bcbcbc" ] ]
                             "2 часа" ] ] ] ] ]

let view model dispatch =
    let isSyncing = Option.isNone model.status
    view [ ViewProperties.Style [ Flex 1. ] ] [
        UI.list model.items (function
            | Actual post -> viewItem post dispatch
            | Old post -> viewItem post dispatch
            | Divider -> viewNextButton dispatch isSyncing) (function
            | Actual post -> string post.id
            | Old post -> string post.id
            | Divider -> "divider") [ FlatListProperties.OnRefresh (dispatch <! Refresh)
                                      FlatListProperties.Refreshing false ]
        UI.reloadButton (Array.isEmpty model.syncState.preloaded)
            "New posts" (always ApplyUpdate >> dispatch)
        UI.loadingView isSyncing ]
