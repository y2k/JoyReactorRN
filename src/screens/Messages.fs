module MessagesScreen 

open System
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open Elmish
open JoyReactor
open JoyReactor.Types

type Model = { messages: ListViewDataSource<Message> }
type Msg = MessagesMsg of Result<Message[], String>

let init userName =
    { messages = emptyDataSource() }, 
    Cmd.ofPromise (Service.loadMessages userName) MessagesMsg

let private itemView _ = view [] []

let view model =
    listView
        model.messages 
        [ ViewProperties.Style [ Flex 1. ]
          ListViewProperties.RenderRow
              (Func<_,_,_,_,_>(fun (i: Message) _ _ _ -> itemView i))
          ListViewProperties.RenderSeparator
              (Func<_,_,_,_>(fun _ _ _ -> view [ ViewProperties.Style [ Height 1.; BackgroundColor "#f8f8f8" ] ] [])) ]