module MessagesScreen 

open System
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open Elmish
open JoyReactor
open JoyReactor.Types
open JoyReactor.CommonUi

type Model = { messages: ListViewDataSource<Message> }
type Msg = MessagesMsg of Result<Message[], String>

let init userName =
    { messages = emptyDataSource() }, 
    Cmd.ofPromise (Service.loadMessages userName) MessagesMsg

let private itemView _ = view [] []

// let myListView<'a> (items: ListViewDataSource<'a>) f =
//     listView
//         items
//         [ ViewProperties.Style [ Flex 1. ]
//           ListViewProperties.RenderRow
//               (Func<_,_,_,_,_>(fun (i: 'a) _ _ _ -> f i))
//           ListViewProperties.RenderSeparator
//               (Func<_,_,_,_>(fun _ _ _ -> view [ ViewProperties.Style [ Height 1.; BackgroundColor "#f8f8f8" ] ] [])) ]

let view model =
    myListView model.messages itemView
    // listView
    //     model.messages 
    //     [ ViewProperties.Style [ Flex 1. ]
    //       ListViewProperties.RenderRow
    //           (Func<_,_,_,_,_>(fun (i: Message) _ _ _ -> itemView i))
    //       ListViewProperties.RenderSeparator
    //           (Func<_,_,_,_>(fun _ _ _ -> view [ ViewProperties.Style [ Height 1.; BackgroundColor "#f8f8f8" ] ] [])) ]