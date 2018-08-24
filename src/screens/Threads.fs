module ThreadsScreen

open System
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish

open JoyReactor
open JoyReactor.Utils
open JoyReactor.Types
open JoyReactor.CommonUi

type Model = 
    { items: Message []
      status: Result<Unit, Exception> option }
type Msg = 
    | ThreadsFromCache of Result<Message [], Exception>
    | ThreadsFromWeb of Result<Message [], Exception>
    | ThreadSelected of String
    | Refresh

let init : Model * Cmd<Msg> = 
    { items = [||]; status = None }, 
    Cmd.ofEffect Service.loadThreadsFromCache ThreadsFromCache

let update model msg =
    match msg with
    | ThreadsFromCache (Ok x) ->
        { model with items = x }, 
        Cmd.ofEffect Service.loadThreadsFromWeb ThreadsFromWeb
    | ThreadsFromCache (Error e) -> log e model, Cmd.none
    | ThreadsFromWeb (Ok x) ->
        { model with
            items = x
            status = Some <| Ok () }, Cmd.none
    | ThreadsFromWeb (Error e) -> 
        { model with status = log e (Some <| Error e) }, Cmd.none
    | Refresh -> 
        { model with  status = None }, Cmd.ofEffect Service.loadThreadsFromWeb ThreadsFromWeb
    | _ -> model, Cmd.none

let private itemView dispatch i =
    touchableHighlight 
        [ TouchableHighlightProperties.Style [ Margin 4. ] 
          TouchableHighlightProperties.ActiveOpacity 0.7
          OnPress (always (ThreadSelected i.userName) >> dispatch) ]
        [ view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding 8. ] ]
             [ image [ ImageProperties.Style [ Width 48.; Height 48.; BorderRadius 24.; MarginRight 8. ]
                       Source [ Uri i.userImage ] ]  
               view [ ViewProperties.Style [ Flex 1. ] ]
                    [ text [ TextProperties.Style [ FontWeight FontWeight.Bold; TextStyle.Color "#404040"; FontSize 15. ] ] 
                           i.userName
                      text [ TextProperties.Style [ TextStyle.Color "#808080"; FontSize 15. ]
                             TextProperties.NumberOfLines 2. ] 
                           i.text
                      text [ TextProperties.Style [ AlignSelf Alignment.FlexEnd; TextStyle.Color "#bdbdbd" ] ] 
                           (longToTimeDelay i.date) ] ] ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] [
        myFlatList 
            model.items (itemView dispatch) (fun x -> x.userName) 
            [ FlatListProperties.OnRefresh (Func<_,_>(fun _ -> dispatch Refresh))
              FlatListProperties.Refreshing false ]
        loadingView <| Option.isNone model.status ]
