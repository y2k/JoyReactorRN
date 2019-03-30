module MessagesScreen

open Elmish
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open JoyReactor
open JoyReactor.Types

module UI = JoyReactor.CommonUi
module Cmd = JoyReactor.Services.Cmd
module S = JoyReactor.Services

type Model = { messages : Message []; isBusy : bool }

type Msg =
    | MessagesMsg of Result<Message [], exn>
    | SendMessage
    | SendMessageResult of Result<Unit, exn>

let init userName =
    { messages = [||]; isBusy = false },
    (S.loadMessages userName) |> Cmd.ofEff MessagesMsg

let update (model : Model) msg =
    match msg with
    | MessagesMsg(Ok x) -> { model with messages = x }, Cmd.none
    | MessagesMsg(Error e) -> log (sprintf "%O" e) model, Cmd.none
    | SendMessage -> { model with isBusy = true }, Cmd.none
    | SendMessageResult(Ok _) -> { model with isBusy = false }, Cmd.none
    | SendMessageResult(Error e) -> log (sprintf "%O" e) { model with isBusy = false }, Cmd.none

let private itemView x =
    let lp, rp = if x.isMine then 100., 0. else 0., 100.
    view [ ViewProperties.Style [ PaddingLeft lp; PaddingRight rp ] ] [
        text [] x.text
        text [] x.userName
        text [ TextProperties.Style [ AlignSelf Alignment.FlexEnd ] ] <| sprintf "%O" x.date ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] [
        UI.list model.messages itemView (fun x -> sprintf "%O" x.date) [ FlatListProperties.Inverted true ]
        view [ ViewProperties.Style [ FlexDirection FlexDirection.Row ] ] [
            textInput [ TextInput.Style [ Flex 1. ] ] "Message text"
            (if model.isBusy
                then view [] []
                else UI.button "SEND" (dispatch <! SendMessage)) ] ]
