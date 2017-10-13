module PostScreen

open JoyReactor
open Elmish

open Fable.Core
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
module R = Fable.Helpers.ReactNative
module S = JoyReactor.Service

[<Pojo>]
type Model =
    { post : Post option 
      error : string option }

[<Pojo>]
type Msg =
| LoadPost of int
| LoadPostResult of Result<Post, string>

let init id =
    { post = None; error = None }, Cmd.ofMsg (LoadPost id)

let update model msg =
    match msg with
    | LoadPost id ->
        model, Cmd.ofPromise (S.loadPost id) LoadPostResult
    | LoadPostResult (Ok post) -> 
        { model with post = Some post }, Cmd.none
    | LoadPostResult (Error error) -> 
        { model with error = Some error }, Cmd.none

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

let viewItem (comment : Comment) = 
      view [ Styles.home ] [
          image [ Styles.image
                  Source [ Uri comment.image.url ] ]
          view [ ViewProperties.Style [ Flex 1. ] ] [
              text [ NumberOfLines 3.
                     TextProperties.Style [ Color "#999" ] ] comment.text
              view [ Styles.panel ] [
                  text [ TextProperties.Style [ FontFamily "icomoon"; Color "#ffb100" ] ] "\ue8dc"
                  text [ TextProperties.Style [ MarginLeft 8.; Color "#616161" ] ] 
                       (string comment.rating) 
              ] ] ]

let view model dispatch =
    let contentView =
        match model with
        | { error = Some e } -> text [] ("ERROR: " + e)
        | { post = Some post } ->
              scrollView [] ([   image [ ImageProperties.Style [ Height 200. ] 
                                         Source [ Uri post.image.Value.url ] ]
                                 text [ TextProperties.Style [ Padding 13. ] ] "Лучшие комментарии:"
                             ] @ List.map viewItem post.comments)
        | _ -> 
              activityIndicator [ ActivityIndicator.Style [ FlexStyle.Flex 1. ]
                                  ActivityIndicator.Size Size.Large
                                  ActivityIndicator.Color "#ffb100" ]
    view [ ViewProperties.Style [ Flex 1.; BackgroundColor "#fafafa" ] ] 
         [ contentView ]