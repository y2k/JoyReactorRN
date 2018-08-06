module PostScreen

open System
open Fable.Core
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNative
open Elmish

open JoyReactor
open JoyReactor.Types

[<Pojo>]
type Model =
    { post  : Post option 
      error : string option }

[<Pojo>]
type Msg =
| LoadPost of int
| PostLoaded of Post
| LoadPostResult of Result<Post, Exception>
| OpenInWeb

let init id =
    { post = None; error = None }, 
    ReactiveStore.loadPost id |> Cmd.ofSub |> Cmd.map PostLoaded

let update model msg =
    match msg with
    | LoadPost id ->
        model, Cmd.ofEffect (Service.loadPost id) LoadPostResult
    | LoadPostResult (Ok post) -> 
        { model with post = Some post }, Cmd.none
    | LoadPostResult (Error error) -> 
        { model with error = Some <| string error }, Cmd.none
    | PostLoaded post -> 
        { model with post = Some post }, Cmd.none
    | OpenInWeb -> 
        model,
        model.post
        |> Option.map (fun x -> x.id |> sprintf "http://m.joyreactor.cc/post/%i" |> Platform.openUrl |> Cmd.ofEffect0)
        |> Option.defaultValue Cmd.none

module private Styles =
    let home = ViewProperties.Style [ PaddingBottom 15.
                                      PaddingHorizontal 13.
                                      FlexDirection FlexDirection.Row ]
    let image = ImageProperties.Style [ MarginRight 13.
                                        Width 36.
                                        Height 36.
                                        BorderRadius 18. ]
    let panel = ViewProperties.Style [ FlexDirection FlexDirection.Row
                                       MarginTop 8.
                                       AlignSelf Alignment.FlexEnd ]

let viewAttachments (comment : Comment) =
    view [ Styles.panel ]
         (comment.attachments
          |> Array.toList
          |> List.map (
              fun a ->
                  image [ ImageProperties.Style [ Width 80.; Height 80. ]
                          Source [ Uri <| Image.normilize a.image.url 80. 80. ] ] ) )

let viewItem (comment : Comment) = 
      view [ Styles.home ] 
           [ image [ Styles.image
                     Source [ Uri comment.image.url ] ]
             view [ ViewProperties.Style [ Flex 1. ] ] 
                  [ text [ TextProperties.Style [ TextStyle.Color "black"; FontWeight FontWeight.Bold ] ]
                         comment.userName
                    text [ NumberOfLines 3.; TextProperties.Style [ TextStyle.Color "#999" ] ] 
                         comment.text
                    viewAttachments comment
                    view [ Styles.panel ] 
                         [ text [ TextProperties.Style [ FontFamily "icomoon"; TextStyle.Color "#ffb100" ] ] 
                                "\ue8dc"
                           text [ TextProperties.Style [ MarginLeft 8.; TextStyle.Color "#616161" ] ] 
                                (string comment.rating) ] ] ]

let view model dispatch =
    let contentView =
        match model with
        | { error = Some e } -> text [] ("Ошибка: " + e)
        | { post = Some post } ->
              scrollView [] ([   image [ ImageProperties.Style [ Height 300. ] 
                                         Source [ Uri post.image.Value.url ] ]
                                 button [ ButtonProperties.Title "Открыть в браузере"
                                          ButtonProperties.OnPress (fun _ -> dispatch OpenInWeb) ] []
                                 text [ TextProperties.Style [ Padding 13. ] ] "Лучшие комментарии:"
                             ] @ (Array.toList <| Array.map viewItem post.comments))
        | _ -> 
              activityIndicator [ ActivityIndicator.Style [ Flex 1. ]
                                  ActivityIndicator.Size Size.Large
                                  ActivityIndicator.Color "#ffb100" ]
    view [ ViewProperties.Style [ Flex 1.; BackgroundColor "#fafafa" ] ] 
         [ contentView ]