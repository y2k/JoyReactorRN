module PostScreen

open Elmish
open Fable.ReactNative.Helpers
open Fable.ReactNative.Props
open JoyReactor
open JoyReactor.Types
module UI = JoyReactor.CommonUi
module S = JoyReactor.Services
type LocalDb = JoyReactor.CofxStorage.LocalDb

type Model = { post : Post option; error : string option; id : int }

type Msg =
    | PostLoaded of Post option
    | RefreshComplete of Result<unit, exn>
    | OpenInWeb
    | OpenTag of Source

let sub id (db : LocalDb) = PostLoaded <| Map.tryFind id db.posts 

let init id = { post = None; error = None; id = id }, (S.runSyncEffect ^ SyncDomain.syncPost id) |> Cmd.ofEffect RefreshComplete

let update model = function
    | PostLoaded x -> { model with post = x }, Cmd.none
    | RefreshComplete(Ok _) -> { model with error = None }, Cmd.none
    | RefreshComplete(Error x) -> { model with error = Some <| string x }, Cmd.none
    | OpenInWeb ->
        model,
        sprintf "http://m.%s/post/%i" UrlBuilder.domain model.id
        |> Platform.openUrl |> Cmd.ofEffect0
    | _ -> model, Cmd.none

module private Styles =
    let home =
        ViewProperties.Style [ PaddingBottom $ 15.
                               PaddingHorizontal $ 13.
                               FlexDirection FlexDirection.Row ]

    let image =
        ImageProperties.Style [ MarginRight $ 13.
                                Width $ 36.
                                Height $ 36.
                                BorderRadius 18. ]

    let panel =
        ViewProperties.Style [ FlexDirection FlexDirection.Row
                               MarginTop $ 8.
                               AlignSelf Alignment.FlexEnd ]

let viewAttachments (comment : Comment) =
    view [ Styles.panel ]
        (comment.attachments
         |> Array.toList
         |> List.map (fun a ->
                image [ ImageProperties.Style [ Width $ 80.; Height $ 80. ]
                        Source <| remoteImage [ Uri <| Image.normilize a.image.url 80. 80. ] ]))

let viewItem (comment : Comment) =
    view [ Styles.home ] [
        image [ Styles.image; Source <| remoteImage [ Uri comment.image.url ] ]
        view [ ViewProperties.Style [ Flex 1. ] ] [
            text [ TextProperties.Style [ TextStyle.Color "black"; FontWeight FontWeight.Bold ] ]
                comment.userName
            text [ NumberOfLines 3.; TextProperties.Style [ TextStyle.Color "#999" ] ]
                comment.text
            viewAttachments comment
            view [ Styles.panel ] [
                UI.iconView
                text [ TextProperties.Style [ MarginLeft $ 8.; TextStyle.Color UI.Colors.darkGray ] ]
                    (string comment.rating) ] ] ]

let viewPostAttachments (post : Post) _ =
    view [ ViewProperties.Style [ FlexDirection FlexDirection.Row ] ]
        (post.attachments
         |> Array.toList
         |> List.map (fun a ->
                image [ ImageProperties.Style [ Width $ 80.; Height $ 80.; Margin $ 2. ]
                        Source <| remoteImage [ Uri <| Image.normilize a.image.url 80. 80. ] ]))

let viewTags post dispatch =
    post.tags
    |> Array.truncate 6
    |> Array.toList
    |> List.map (fun x -> UI.roundButton x (dispatch <! OpenTag(TagSource x)) [ PaddingHorizontal $ 8. ])
    |> flip List.append [ UI.roundButton "Все теги" ignore [ PaddingHorizontal $ 8. ] ]
    |> view [ ViewProperties.Style [ FlexWrap FlexWrap.Wrap; FlexDirection FlexDirection.Row ] ]

let viewContent post dispatch =
    scrollView [] [
         yield! 
            post.image 
            |> Array.map ^ fun x ->
                image [ ImageProperties.Style [ Height $ 300. ]; Source ^ remoteImage [ Uri x.url ] ]
         yield button [ ButtonProperties.Title "Открыть в браузере"
                        ButtonProperties.OnPress(dispatch <! OpenInWeb) ] []
         yield text [ TextProperties.Style [ Padding $ 13. ] ] "Приложения:"
         yield viewPostAttachments post dispatch
         yield text [ TextProperties.Style [ Padding $ 13. ] ] "Теги:"
         yield viewTags post dispatch
         yield text [ TextProperties.Style [ Padding $ 13. ] ] "Лучшие комментарии:"
         yield! Array.map viewItem post.comments ]

let view model dispatch =
    let contentView =
        match model with
        | { error = Some e } -> text [] ("Ошибка: " + e)
        | { post = Some post } -> viewContent post dispatch
        | _ ->
            activityIndicator [ ViewProperties.Style [ Flex 1. ]
                                ActivityIndicator.Size Size.Large
                                ActivityIndicator.Color UI.Colors.orange ]
    view [ ViewProperties.Style [ Flex 1.; BackgroundColor "#fafafa" ] ] [
        contentView ]
