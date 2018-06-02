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
    | MessagesMsg of Result<Message[], String>
    | SendMessage
    | SendMessageResult of Result<Unit, String>

let init userName =
    { messages = [||]; isBusy = false }, 
    Cmd.ofPromise_ (Service.loadMessages userName) MessagesMsg

let update model msg = 
    match msg with
    | MessagesMsg (Ok x) -> { model with messages = x }, Cmd.none
    | MessagesMsg (Error e) -> log (sprintf "%O" e) model, Cmd.none
    | SendMessage -> { model with isBusy = true }, 
                     Cmd.ofPromise_ Service.testReloadMessages SendMessageResult
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

let private listView<'a> (items: 'a[]) f fid =
    flatList 
        items
        [ FlatListProperties.RenderItem(Func<_,_>(fun x -> f <| unbox<'a> x.item))
          FlatListProperties.KeyExtractor(Func<_,_,_>(fun x _ -> fid <| unbox<'a> x))
          MyFlatListProperties.Inverted true ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] 
         [ listView model.messages itemView (fun x -> sprintf "%O" x.date)
           view [ ViewProperties.Style [ FlexDirection FlexDirection.Row ] ]
                [ textInput [] "Message text"
                  (if model.isBusy then view [] [] 
                   else testButton "SEND" (fun _ -> dispatch SendMessage)) ] ]