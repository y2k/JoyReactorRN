module Home

open System
open Fable.Import.JS
open Fable.Helpers.ReactNative
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish
open JoyReactor

module S = Service

type PostState = Actual of Post | Divider | Old of Post

type Msg = 
    | LoadPosts of Source
    | LoadResult of Result<Post list * int option, string>
    | LoadNextPage
    | OpenPost of Post

type Model = 
    { posts : ListViewDataSource<PostState>
      rawPosts : Post list
      cache: PostsWithLevels
      nextPage : int option }

let init =
    { posts = emptyDataSource(); rawPosts = []; nextPage = None; cache = { actual = []; old = [] } }, 
    Cmd.ofMsg (LoadPosts FeedSource)

let postsToItems xs =
    []
    |> List.append (xs.old |> List.map Old)
    |> List.append [ Divider ]
    |> List.append (xs.actual |> List.map Actual)
    |> List.toArray

let update model msg : Model * Cmd<Msg> = 
    match msg with
    | LoadPosts source ->
        model, Cmd.ofPromise (S.loadPosts source model.nextPage) LoadResult
    | LoadResult (Ok (posts, nextPage)) ->
        let merged = Domain.mergeNextPage model.cache posts
        { posts = updateDataSource (postsToItems merged) model.posts
          rawPosts = posts
          cache = merged
          nextPage = nextPage }, Cmd.none
    | LoadResult (Error _) ->
        model, Cmd.none
    | LoadNextPage ->
        model, Cmd.ofPromise (S.loadPosts FeedSource model.nextPage) LoadResult
    | _ -> model, Cmd.none

let viewNextButton dispatch =
    touchableOpacity 
        [ TouchableWithoutFeedbackProperties.Style 
            [ Margin 4. 
              BackgroundColor "#e49421"
              BorderRadius 4.
              Overflow Overflow.Hidden ]
          TouchableWithoutFeedbackProperties.OnPress (fun _ -> dispatch LoadNextPage) ]
        [ text 
            [ TextProperties.Style 
                [ FontWeight FontWeight.Bold
                  FontSize 13.
                  TextAlign TextAlignment.Center
                  Padding 15.
                  Color "white" ] ] 
            "Load next page" ]

let todo attachment limitWidth = 
    let aspect = max 1.2 attachment.aspect
    let w = limitWidth
    let h = w / aspect
    Image.normilize attachment.url w h, h

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
          OnPress (fun _ -> OpenPost post |> dispatch) ]
        [ view [ ViewProperties.Style 
                     [ AlignItems ItemAlignment.Stretch
                       BackgroundColor "white"
                       BorderColor "#eee"
                       BorderWidth 1.
                       BorderRadius 8.
                       Overflow Overflow.Hidden ] ] 
               [ viewPostImage post
                 view [ ViewProperties.Style
                            [ FlexDirection FlexDirection.Row; Margin 9. ] ] 
                      [ image [ ImageProperties.Style 
                                  [ Width 36.; Height 36.; BorderRadius 18.; MarginRight 9. ]
                                Source [ Uri post.userImage.url ] ]
                        view [ ViewProperties.Style [ Flex 1. ] ] 
                             [ text [ TextProperties.Style 
                                          [ FontWeight FontWeight.Bold; FontSize 14.; Color "#616161" ] ] 
                                    post.userName
                               view [ ViewProperties.Style 
                                          [ AlignSelf Alignment.FlexEnd
                                            FlexDirection FlexDirection.Row ] ] 
                                    [ text [ TextProperties.Style [ FontFamily "icomoon"; Color "#ffb100" ] ] 
                                           "\ue8b5"
                                      text [ TextProperties.Style [ MarginLeft 8.; Color "#bcbcbc" ] ] 
                                           "2 часа" ] ] ]
            ]
        ]

let view model dispatch = 
    listView model.posts [
        ListViewProperties.RenderRow
            (Func<_,_,_,_,_>(fun (i: PostState) _ _ _ ->
                match i with
                | Actual x -> viewItem x dispatch
                | Old x -> viewItem x dispatch
                | Divider -> viewNextButton dispatch
                ))
    ]