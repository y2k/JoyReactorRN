module Home

open System
open Fable.Import.JS
open Fable.Helpers.ReactNative
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish
open JoyReactor

module S = Service

type PostState =
| Actual of Post
| Divider
| Old of Post

type Msg = 
    | LoadPosts of Source
    | LoadResult of Result<Post list * int option, string>

type Model = 
    { posts : ListViewDataSource<PostState>
      nextPage : int option }

let init =
    { posts = emptyDataSource(); nextPage = None }, Cmd.ofMsg (LoadPosts FeedSource)

let postsToItems posts =
    posts 
    |> List.map Actual 
    |> fun xs -> xs @ [Divider]
    |> List.toArray

let update model msg : Model * Cmd<Msg> = 
    match msg with
    | LoadPosts _ ->
        model, Cmd.ofPromise (S.loadPosts 0) LoadResult
    | LoadResult (Ok (posts, nextPage)) ->
        { posts = updateDataSource (posts |> postsToItems) model.posts
          nextPage = nextPage }, Cmd.none
    | LoadResult (Error _) ->
        model, Cmd.none

let nextButton () =
    touchableOpacity 
        [ TouchableWithoutFeedbackProperties.Style 
            [ Margin 4. 
              BackgroundColor "#e49421"
              BorderRadius 4.
              Overflow Overflow.Hidden ] ] 
        [ text 
            [ TextProperties.Style 
                [ FontWeight FontWeight.Bold
                  FontSize 13.
                  TextAlign TextAlignment.Center
                  Padding 15.
                  Color "white" ] ] 
            "Load next page" ]

let view state = 
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
                | Divider -> nextButton ()
                | _ -> text [] "=== STUB ==="
                ))
    ]