module PostScreen

open Elmish
open Fable.Core
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open JoyReactor
open JoyReactor.Types

module UI = JoyReactor.CommonUi
module Cmd = JoyReactor.Services.Cmd
module S = JoyReactor.Services

type Model = { post : Post option; error : string option }

type Msg =
    | LoadPost of int
    | PostLoaded of Result<Post, exn>
    | LoadPostResult of Result<Post, exn>
    | OpenInWeb
    | OpenTag of Source

let init id =
    { post = None; error = None },
    S.loadPost id |> Cmd.ofEff PostLoaded

let update model msg =
    match msg with
    | LoadPost id -> model, S.loadPost id |> Cmd.ofEff LoadPostResult
    | LoadPostResult(Ok post) -> { model with post = Some post }, Cmd.none
    | LoadPostResult(Error error) -> { model with error = Some <| string error }, Cmd.none
    | PostLoaded(Ok post) -> { model with post = Some post }, Cmd.none
    | OpenInWeb ->
        model,
        model.post
        |> Option.map (fun x ->
               x.id
               |> sprintf "http://m.%s/post/%i" UrlBuilder.domain
               |> Platform.openUrl
               |> Cmd.ofEffect0)
        |> Option.defaultValue Cmd.none
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
         yield image [ ImageProperties.Style [ Height $ 300. ]
                       Source <| remoteImage [ Uri post.image.Value.url ] ]
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
