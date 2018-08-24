module Home

open System
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNative
open Elmish

open JoyReactor
open JoyReactor.Utils
open JoyReactor.Types
open JoyReactor.CommonUi

type PostState = Actual of Post | Divider | Old of Post

type Msg = 
    | PostsLoaded of Result<PostsWithLevels, Exception>
    | LoadNextPage
    | OpenPost of Post
    | Refresh
    | ApplyUpdate

type Model = 
    { syncState : PostsWithLevels
      items     : PostState []
      status    : Result<unit, exn> option
      source    : Source }

let init source =
    let cmd1 = Cmd.ofEffect (ReactiveStore.getCachedPosts source) PostsLoaded
    let cmd2 = Cmd.ofEffect (ReactiveStore.syncFirstPage source) PostsLoaded

    { syncState = PostsWithLevels.empty ; items = [||]; status = None; source = source }, 
    Cmd.batch [cmd1; cmd2]

let postsToPostStates posts =
    if Array.isEmpty posts.old && Array.isEmpty posts.actual 
    then [||]
    else Array.concat 
            [ posts.actual |> Array.map Actual
              (if (not <| Array.isEmpty posts.preloaded) || Array.isEmpty posts.actual then [||] else [| Divider |])
              posts.old |> Array.map Old ]

let update model msg : Model * Cmd<Msg> = 
    match msg with
    | PostsLoaded (Ok x) ->
        { model with items = postsToPostStates x
                     syncState = x
                     status = if Array.isEmpty x.actual then None else Some <| Ok () }, Cmd.none
    | PostsLoaded (Error e) ->
        log e { model with status = Some <| Error e }, Cmd.none
    | ApplyUpdate -> 
        model, Cmd.ofEffect (ReactiveStore.applyUpdate model.source model.syncState) PostsLoaded
    | Refresh ->
        if Array.isEmpty model.syncState.preloaded 
            then { model with status = None }, Cmd.ofEffect (ReactiveStore.reset model.source) PostsLoaded
            else model, Cmd.ofMsg ApplyUpdate
    | LoadNextPage -> 
        { model with status = None }, 
        Cmd.ofEffect (ReactiveStore.syncNextPage model.source model.syncState) PostsLoaded
    | _ -> model, Cmd.none

module private Styles =
    let nextButtonOutter enabled =
        TouchableWithoutFeedbackProperties.Style 
            [ Margin 4. 
              BackgroundColor (if enabled then "#e49421" else "#e4942100")
              BorderRadius 4.
              Overflow Overflow.Hidden ]
    let nextButtonInner =
        TextProperties.Style 
            [ FontWeight FontWeight.Bold
              FontSize 13.
              TextAlign TextAlignment.Center
              Padding 15.
              TextStyle.Color "white" ]
    let card =
        ViewProperties.Style 
            [ AlignItems ItemAlignment.Stretch
              BackgroundColor "white"
              BorderColor "#eee"
              BorderWidth 1.
              BorderRadius 8.
              Overflow Overflow.Hidden ]
    let avatar =
        ImageProperties.Style 
            [ Width 36.; Height 36.; BorderRadius 18.; MarginRight 9. ]
    let userName =
        TextProperties.Style 
            [ FontWeight FontWeight.Bold; FontSize 14.; TextStyle.Color "#616161" ]

let viewNextButton dispatch isSyncing =
    let onPress = if isSyncing then ignore else (fun _ -> dispatch LoadNextPage)
    touchableOpacity
        [ Styles.nextButtonOutter <| not isSyncing
          OnPress onPress ]
        [ text [ Styles.nextButtonInner ] "Load next page" ]

let viewPostImage post =
    post.image 
    |> Option.map (Image.urlWithHeight (Globals.Dimensions.get("screen").width))
    |> function
       | Some (img, h) ->
             image [ ImageProperties.Style 
                         [ Height h; BorderTopLeftRadius 8.; BorderTopRightRadius 8. ]
                     Source [ Uri img ] ]
       | None -> view [] []

let viewItem post dispatch =
    touchableHighlight 
        [ TouchableHighlightProperties.Style [ Margin 4. ] 
          TouchableHighlightProperties.ActiveOpacity 0.7
          OnPress (always (OpenPost post) >> dispatch) ]
        [ view [ Styles.card ]
               [ viewPostImage post
                 view [ ViewProperties.Style
                            [ FlexDirection FlexDirection.Row; Margin 9. ] ] 
                      [ image [ Styles.avatar
                                Source [ Uri post.userImage.url ] ]
                        view [ ViewProperties.Style [ Flex 1. ] ] 
                             [ text [ Styles.userName ] 
                                    post.userName
                               view [ ViewProperties.Style 
                                          [ AlignSelf Alignment.FlexEnd
                                            FlexDirection FlexDirection.Row ] ] 
                                    [ text [ TextProperties.Style [ FontFamily "icomoon"; TextStyle.Color "#ffb100" ] ] 
                                           "\ue8b5"
                                      text [ TextProperties.Style [ MarginLeft 8.; TextStyle.Color "#bcbcbc" ] ] 
                                           "2 часа" ] ] ] ] ]

let view model dispatch = 
    let isSyncing = Option.isNone model.status
    view [ ViewProperties.Style [ Flex 1. ] ]
         [ myFlatList
               model.items
               (function
                | Actual post -> viewItem post dispatch
                | Old post -> viewItem post dispatch
                | Divider -> viewNextButton dispatch isSyncing)
               (function
                | Actual post -> string post.id
                | Old post -> string post.id
                | Divider -> "divider")
               [ FlatListProperties.OnRefresh (Func<_,_>(fun () -> dispatch Refresh))
                 FlatListProperties.Refreshing false ]
           reloadButton (Array.isEmpty model.syncState.preloaded) "Есть новые посты" (always ApplyUpdate >> dispatch)
           loadingView isSyncing ]
