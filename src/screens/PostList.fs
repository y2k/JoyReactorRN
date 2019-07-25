module PostsComponent

open Elmish
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open JoyReactor
open JoyReactor.Types
type LocalDb = JoyReactor.CofxStorage.LocalDb
module UI = JoyReactor.CommonUi
module Effects = JoyReactor.Services.Storage'

module Effects =
    open JoyReactor.Services
    type NextPosts = Post [] * int option

    let sync (s : Source) (p : int option) (f : LocalDb -> NextPosts -> LocalDb) : unit Async = async {
        let! (x, y) = loadPosts s p
        let x = Seq.toArray x
        do! Storage'.update ^ fun db -> f db (x, y), () }

type PostState = | Actual of Post | Divider | Old of Post

type Msg =
    | PostsMsg of PostsWithLevels
    // | PostsPreloaded of Result<unit, exn>
    | SyncResultMsg of Result<unit, exn>
    //
    | ApplyUpdate
    //
    | Refresh
    | LoadNextPage
    | OpenPost of Post

type Model =
    { items : PostState []
      hasNew : bool
      loading : bool
      // ---
      nextPage : int option
      source : Source }

let init source =
    { source = source; items = [||]; hasNew = false; nextPage = None; loading = true },
    Effects.sync source None ^ fun db (xs, np) ->
        let x = Map.tryFind source db.feeds' |> Option.defaultValue PostsWithLevels.empty
        let x =
            if Seq.isEmpty x.actual
                then { x with actual = xs; nextPage = np }
                else { x with preloaded = xs; nextPage = np }
        let x = { db with feeds' = Map.add source x db.feeds' }
        Log.log ^ sprintf "INIT %A" x
        x
    |> Cmd.ofEffect SyncResultMsg

let sub source (db : LocalDb) =
    Map.tryFind source db.feeds' |> Option.defaultValue PostsWithLevels.empty |> PostsMsg

module F =
    let mkItems (ps : PostsWithLevels) : PostState [] =
        if Seq.isEmpty ps.preloaded
            then Array.concat [ ps.actual |> Array.map Actual; [| Divider |]; ps.old |> Array.map Old ]
            else Array.concat [ ps.actual |> Array.map Actual; ps.old |> Array.map Old ]

    let filterNotIn target xs =
        let ids =
            target
            |> Seq.map ^ fun x -> x.id
            |> Set.ofSeq
        xs |> Array.filter ^ fun x -> not ^ Set.contains x.id ids

    let mergeApply (db : LocalDb) source =
        let x = Map.tryFind source db.feeds' |> Option.defaultValue PostsWithLevels.empty
        let x = { x with old = Array.concat [ x.actual; x.old ] |> filterNotIn x.preloaded }
        let x = { x with actual = x.preloaded; preloaded = [||] }
        { db with feeds' = Map.add source x db.feeds' }

    let mergeNextPage source (db : LocalDb) (posts, nextPage) =
        let x = Map.tryFind source db.feeds' |> Option.defaultValue PostsWithLevels.empty
        let x = { x with 
                    actual = Array.concat [ x.actual; filterNotIn x.actual posts ]
                    old = filterNotIn posts x.old
                    nextPage = nextPage }
        { db with feeds' = Map.add source x db.feeds' }

    let mergeFirstPage source (db : LocalDb) posts =
        { db with feeds' = Map.remove source db.feeds' }
        |> fun db -> mergeNextPage source db posts

let update model = function
    | PostsMsg x ->
        Log.log (sprintf "PostsMsg, X = %O" x)
        { model with items = F.mkItems x; hasNew = not <| Array.isEmpty x.preloaded; nextPage = x.nextPage },
        Cmd.none
    | SyncResultMsg(Ok _) -> { model with loading = false }, Cmd.none
    //
    | ApplyUpdate ->
        model,
        (Services.Storage'.update ^ fun db -> F.mergeApply db model.source, ()) |> Cmd.ofEffect0
    | LoadNextPage ->
        { model with loading = true },
        Effects.sync model.source model.nextPage ^ F.mergeNextPage model.source |> Cmd.ofEffect SyncResultMsg
    | Refresh ->
        model,
        Effects.sync model.source None ^ F.mergeFirstPage model.source |> Cmd.ofEffect SyncResultMsg
    //
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
        |> Option.map (Image.urlWithHeight (Fable.Import.ReactNative.Globals.Dimensions.get("screen").width))
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
            [ FlatListProperties.OnRefresh(dispatch <! Refresh)
              FlatListProperties.Refreshing false ]
        UI.reloadButton (not model.hasNew) "New posts" (dispatch <! ApplyUpdate)
        UI.loadingView model.loading ]
