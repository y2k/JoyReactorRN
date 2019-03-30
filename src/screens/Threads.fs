module ThreadsScreen

open Elmish
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open JoyReactor
open JoyReactor.Types

module UI = JoyReactor.CommonUi
module Cmd = JoyReactor.Free.Cmd
module Service = JoyReactor.Free.Service

type Model =
    { items : Message []
      status : Result<Unit, exn> option }

type Msg =
    | ThreadsFromCache of Result<Message [], exn>
    | ThreadsFromWeb of Result<Message [], exn>
    | ThreadSelected of string
    | Refresh

let init : Model * Cmd<Msg> =
    { items = [||]
      status = None }, Cmd.ofEffect Service.loadThreadsFromCache ThreadsFromCache

let update model msg =
    match msg with
    | ThreadsFromCache(Ok x) -> { model with items = x }, Cmd.ofEffect Service.loadThreadsFromWeb ThreadsFromWeb
    | ThreadsFromCache(Error e) -> log e model, Cmd.none
    | ThreadsFromWeb(Ok x) ->
        { model with items = x
                     status = Some <| Ok() }, Cmd.none
    | ThreadsFromWeb(Error e) -> { model with status = log e (Some <| Error e) }, Cmd.none
    | Refresh -> { model with status = None }, Cmd.ofEffect Service.loadThreadsFromWeb ThreadsFromWeb
    | _ -> model, Cmd.none

let private itemView dispatch i =
    touchableHighlight [ TouchableHighlightProperties.Style [ Margin 4. ]
                         TouchableHighlightProperties.ActiveOpacity 0.7
                         OnPress(dispatch <! ThreadSelected i.userName) ] [ 
        view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding 8. ] ] [ 
            image [ ImageProperties.Style [ Width 48.
                                            Height 48.
                                            BorderRadius 24.
                                            MarginRight 8. ]
                    Source [ Uri i.userImage ] ]
            view [ ViewProperties.Style [ Flex 1. ] ] [ 
                text [ TextProperties.Style [ FontWeight FontWeight.Bold
                                              TextStyle.Color "#404040"
                                              FontSize 15. ] ] i.userName
                text [ TextProperties.Style [ TextStyle.Color "#808080"; FontSize 15. ]
                       TextProperties.NumberOfLines 2. ] i.text
                text [ TextProperties.Style [ AlignSelf Alignment.FlexEnd
                                              TextStyle.Color "#bdbdbd" ] ] (longToTimeDelay i.date) ] ] ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] [ 
        UI.list model.items (itemView dispatch) (fun x -> x.userName)
            [ FlatListProperties.OnRefresh (System.Func<_, _>(dispatch <! Refresh))
              FlatListProperties.Refreshing false ]
        UI.loadingView <| Option.isNone model.status ]
