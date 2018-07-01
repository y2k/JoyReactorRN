module MessagesScreen 

open System
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish

open JoyReactor
open JoyReactor.Types
open JoyReactor.CommonUi
open JoyReactor.Utils

type Model = { messages: Message[]; isBusy: Boolean }
type Msg = 
    | MessagesMsg of Result<Message[], Exception>
    | SendMessage
    | SendMessageResult of Result<Unit, Exception>

let init userName =
    { messages = [||]; isBusy = false }, 
    Cmd.ofEffect (Service.loadMessages userName) MessagesMsg

let update model msg = 
    match msg with
    | MessagesMsg (Ok x) -> { model with messages = x }, Cmd.none
    | MessagesMsg (Error e) -> log (sprintf "%O" e) model, Cmd.none
    | SendMessage -> { model with isBusy = true }, 
                     Cmd.ofEffect Service.testReloadMessages SendMessageResult
    | SendMessageResult (Ok _) -> { model with isBusy = false }, Cmd.none
    | SendMessageResult (Error e) -> log (sprintf "%O" e) { model with isBusy = false }, Cmd.none

let private itemView x = 
    let lp, rp = if x.isMine then 100., 0. else 0., 100.
    view [ ViewProperties.Style [ PaddingLeft lp; PaddingRight rp ] ] 
         [ text [] x.text
           text [] x.userName 
           text [ TextProperties.Style [ AlignSelf Alignment.FlexEnd ] ] 
                <| sprintf "%O" x.date ]

type MyFlatListProperties<'a> =
    | Inverted of bool
    interface IFlatListProperties<'a>

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] 
         [ myFlatList 
               model.messages itemView (fun x -> sprintf "%O" x.date)
               [ MyFlatListProperties.Inverted true ]
           view [ ViewProperties.Style [ FlexDirection FlexDirection.Row ] ]
                [ textInput [ TextInput.Style [ Flex 1. ] ] "Message text"
                  (if model.isBusy then view [] [] 
                   else testButton "SEND" (fun _ -> dispatch SendMessage)) ] ]