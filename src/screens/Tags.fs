module TagsScreen

open System
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNative
open Elmish

open JoyReactor
open JoyReactor.Types
open JoyReactor.Utils
open JoyReactor.CommonUi

type Model = { tags: Tag []; loaded: Boolean }
type Msg = 
    | FromCache of Result<Tag [], Exception> 
    | FromWeb of Result<Tag [], Exception> 
    | TagsLoaded of Tag [] 
    | OpenPosts of Source 
    | Refresh

let init = 
    let cmd = Cmd.batch [ ReactiveStore.getTagsFromCache |> flip Cmd.ofEffect FromCache
                          ReactiveStore.getTagsFromWeb |> flip Cmd.ofEffect FromWeb ]
    { tags = [||]; loaded = false }, cmd

let addFavorite tags =
    Array.concat
        [ [| { name = "Избранное"; image = "http://img1.joyreactor.cc/pics/avatar/tag/1279" } |]
          tags ]

let tagToSourse tag =
    match tag.name with
    | "Избранное" -> FavoriteSource
    | _ -> TagSource tag.name

let update model = function
    | Refresh             -> { model with loaded = false }, ReactiveStore.getTagsFromWeb |> flip Cmd.ofEffect FromWeb
    | FromCache (Ok tags) -> { model with tags = addFavorite tags }, Cmd.none
    | FromWeb (Ok tags)   -> { model with tags = addFavorite tags; loaded = true }, Cmd.none
    | FromCache (Error e) -> raise e
    | FromWeb (Error e)   -> raise e
    | _                   -> model, Cmd.none

module Styles =
    let image = 
        ImageProperties.Style [ Width 48.; Height 48.; BorderRadius 24.; MarginRight 8. ]
    let label =
        TextProperties.Style [ FontSize 18.; TextStyle.Color "#404040"; AlignSelf Alignment.Center ]

let viewItem dispatch (x : Tag) = 
    touchableOpacity 
        [ ActiveOpacity 0.4
          OnPress (fun _ -> tagToSourse x |> OpenPosts |> dispatch) ] 
        [ view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding 8. ] ] 
               [ image [ Styles.image; Source [ Uri x.image ] ]
                 text [ Styles.label ] x.name ] ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] [
        myFlatList model.tags (viewItem dispatch) (fun x -> x.name) 
                   [ FlatListProperties.OnRefresh (Func<_,_>(fun _ -> dispatch Refresh)) 
                     FlatListProperties.Refreshing false ]
        loadingView <| not model.loaded ]
