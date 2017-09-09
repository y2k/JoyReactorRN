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

type Model = 
    { posts : ListViewDataSource<PostState>
      rawPosts : Post list
      nextPage : int option }

let init =
    { posts = emptyDataSource(); rawPosts = []; nextPage = None }, 
    Cmd.ofMsg (LoadPosts FeedSource)

let postsToItems posts olds =
    posts 
    |> List.map Actual 
    |> fun xs -> List.append xs [Divider]
    |> fun xs -> List.append xs (olds |> List.map Old)
    |> List.toArray

let update model msg : Model * Cmd<Msg> = 
    match msg with
    | LoadPosts source ->
        model, Cmd.ofPromise (S.loadPosts source model.nextPage) LoadResult
    | LoadResult (Ok (posts, nextPage)) ->
        { posts = updateDataSource (postsToItems posts model.rawPosts) model.posts
          rawPosts = posts
          nextPage = nextPage }, Cmd.none
    | LoadResult (Error _) ->
        model, Cmd.none
    | LoadNextPage ->
        model, Cmd.ofPromise (S.loadPosts FeedSource model.nextPage) LoadResult

let nextButton dispatch =
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

let view state dispatch = 
    listView state.posts [
        ListViewProperties.RenderRow
            (Func<_,_,_,_,_>(fun (i: PostState) _ _ _ ->
                match i with
                | Actual x ->
                    let (img, h) = 
                        x.image 
                        |> Option.map (fun x -> 
                            let h = Globals.Dimensions.get("screen").width / (max 1.2 x.aspect)
                            x.url, h) 
                        |> Option.defaultValue ("", 0.)
                    image [ ImageProperties.Style [ Height h ]
                            Source [ Uri img ] ]
                | Divider -> nextButton dispatch
                | _ -> text [] "=== STUB ==="
                ))
    ]