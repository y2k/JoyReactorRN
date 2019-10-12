module MessagesScreen

open Elmish
open Fable.ReactNative.Helpers
open Fable.ReactNative.Props
open JoyReactor
open JoyReactor.Types
module UI = CommonUi
module S = Services
type LocalDb = CofxStorage.LocalDb

type Model = { messages : Message []; isBusy : bool; username : string }

type Msg =
    | MessagesLoaded of Message []
    | SendMessage
    | SendMessageResult of Result<Unit, exn>

let sub username (db : LocalDb) = MessagesLoaded <| Domain.selectMessageForUser username db.messages

let init username = { messages = [||]; isBusy = false; username = username }, Cmd.none

let update (model : Model) msg =
    match msg with
    | MessagesLoaded x -> { model with messages = x }, Cmd.none
    | SendMessage -> { model with isBusy = true }, Cmd.none
    | SendMessageResult(Ok _) -> { model with isBusy = false }, Cmd.none
    | SendMessageResult(Error e) -> log (sprintf "%O" e) { model with isBusy = false }, Cmd.none

let private itemView x =
    let lp, rp = if x.isMine then 100., 0. else 0., 100.
    view [ ViewProperties.Style [ PaddingLeft $ lp; PaddingRight $ rp ] ] [
        text [] x.text
        text [] x.userName
        text [ TextProperties.Style [ AlignSelf Alignment.FlexEnd ] ] <| sprintf "%O" x.date ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] [
        UI.list model.messages itemView (fun x -> sprintf "%O" x.date) [ UI.FlatListPropertiesExt.Inverted true ]
        view [ ViewProperties.Style [ FlexDirection FlexDirection.Row ] ] [
            textInputWithChild [ TextInput.TextInputProperties.Style [ Flex 1. ] ] "Message text"
            (if model.isBusy
                then view [] []
                else UI.button "SEND" (dispatch <! SendMessage)) ] ]
