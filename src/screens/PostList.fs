module PostsComponent

open Elmish
open Fable.ReactNative.Helpers
open Fable.ReactNative.Props
open JoyReactor
open JoyReactor.Types
type LocalDb = CofxStorage.LocalDb
module UI = CommonUi
module R = Services.EffRuntime
module D = MergeDomain

type PostState = Actual of Post | Divider | Old of Post

type Msg =
    | PostsMsg of PostsWithLevels
    | SyncResultMsg of Result<unit, exn>
    | ApplyUpdate
    | ApplyUpdateEnd of Result<unit, exn>
    | Refresh
    | LoadNextPage
    | OpenPost of Post

type Model =
    { items : PostState []
      hasNew : bool
      loading : bool
      nextPage : int option
      source : Source }

let init source =
    { source = source; items = [||]; hasNew = false; nextPage = None; loading = true },
    R.run ^ MergeDomain.premergeFirstPage source None
    |> Cmd.map SyncResultMsg

let sub source (db : LocalDb) =
    Map.tryFind source db.feeds |> Option.defaultValue PostsWithLevels.empty |> PostsMsg

let update model = function
    | PostsMsg x ->
        let mkItems (ps : PostsWithLevels) : PostState [] =
            if Seq.isEmpty ps.preloaded
                then Array.concat [ ps.actual |> Array.map Actual; [| Divider |]; ps.old |> Array.map Old ]
                else Array.concat [ ps.actual |> Array.map Actual; ps.old |> Array.map Old ]
        { model with items = mkItems x; hasNew = not <| Array.isEmpty x.preloaded; nextPage = x.nextPage },
        Cmd.none
    | SyncResultMsg(Ok _) -> { model with loading = false }, Cmd.none
    | ApplyUpdate -> model, D.mergeApply model.source |> R.run |> Cmd.map ApplyUpdateEnd
    | LoadNextPage ->
        { model with loading = true },
        R.run ^ MergeDomain.mergeNextPage model.source model.nextPage |> Cmd.map SyncResultMsg
    | Refresh ->
        model,
        R.run ^ MergeDomain.mergeFirstPage model.source |> Cmd.map SyncResultMsg
    | ApplyUpdateEnd (Ok _) -> model, Cmd.none
    | x -> failwithf "%O" x

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

let viewItem dispatch post =
    let viewPostImage post =
        post.image
        |> Array.tryHead
        |> Option.map ^ Image.urlWithHeight ((Fable.ReactNative.RN.Dimensions.get "screen").width)
        |> function
           | Some(img, h) ->
               image [ ImageProperties.Style [ Height $ h; BorderTopLeftRadius 8.; BorderTopRightRadius 8. ]
                       Source <| remoteImage [ Uri img ] ]
           | None -> view [] []

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

let viewNextButton dispatch isSyncing =
    let onPress = if isSyncing then ignore else dispatch <! LoadNextPage
    touchableOpacity [ Styles.nextButtonOutter <| not isSyncing; OnPress onPress ] [
        text [ Styles.nextButtonInner ] "Load next page" ]

let view model dispatch =
    let mkId = function | Divider -> -1 | Actual x -> x.id | Old x -> x.id
    view [ ViewProperties.Style [ Flex 1. ] ] [
        UI.list model.items
            (function
             | Actual x -> viewItem dispatch x
             | Old x -> viewItem dispatch x
             | Divider -> viewNextButton dispatch model.loading)
            (mkId >> string)
            [ OnRefresh(dispatch <! Refresh); Refreshing false ]
        UI.reloadButton (not model.hasNew) "New posts" (dispatch <! ApplyUpdate)
        UI.loadingView model.loading ]
