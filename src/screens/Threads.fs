module ThreadsScreen

open System
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open Elmish
open JoyReactor
open JoyReactor.Utils
open JoyReactor.Types

type Model = 
    { items: ListViewDataSource<Message>
      status: Result<Unit, String> option }
type Msg = 
    | ThreadsFromCache of Result<Message [], String>
    | ThreadsFromWeb of Result<Message [], String>
    | ThreadSelected of String
    | ReloadThreads

let init: Model * Cmd<Msg> = 
    { items = emptyDataSource(); status = None }, 
    Cmd.ofPromise Service.loadThreadsFromCache ThreadsFromCache

let update model msg =
    match msg with
    | ThreadsFromCache (Ok x) ->
        { model with items = updateDataSource x model.items }, 
        Cmd.ofPromise Service.loadThreadsFromWeb ThreadsFromWeb
    | ThreadsFromCache (Error e) -> log e model, Cmd.none
    | ThreadsFromWeb (Ok x) ->
        { model with
            items = updateDataSource x model.items
            status = Some <| Ok () }, Cmd.none
    | ThreadsFromWeb (Error e) -> 
        { model with status = log e (Some <| Error e) }, Cmd.none
    | ThreadSelected _ -> failwith "Not Implemented"
    | ReloadThreads -> failwith "Not Implemented"

let private itemView i =
    view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding 8. ] ]
         [ image [ ImageProperties.Style [ Width 48.; Height 48.; BorderRadius 24.; MarginRight 8. ]
                   Source [ Uri i.userImage ] ]  
           view [ ViewProperties.Style [ Flex 1. ] ]
                [ text [ TextProperties.Style [ FontWeight FontWeight.Bold; Color "#404040"; FontSize 15. ] ] 
                       i.userName
                  text [ TextProperties.Style [ Color "#808080"; FontSize 15. ]
                         TextProperties.NumberOfLines 2. ] 
                       i.text
                  text [ TextProperties.Style [ AlignSelf Alignment.FlexEnd; Color "#bdbdbd" ] ] 
                       (longToTimeDelay i.date) ] ]

let private listView items =
    listView
        items 
        [ ViewProperties.Style [ Flex 1. ]
          ListViewProperties.RenderRow
              (Func<_,_,_,_,_>(fun (i: Message) _ _ _ -> itemView i))
          ListViewProperties.RenderSeparator
              (Func<_,_,_,_>(fun _ _ _ -> view [ ViewProperties.Style [ Height 1.; BackgroundColor "#f8f8f8" ] ] [])) ]

let statusView status = 
    match status with
    | Some (Ok _) -> view [] []
    | Some (Error _) -> text [] "ERROR"
    | None ->
        activityIndicator 
            [ ViewProperties.Style [ BackgroundColor "#212121"; Padding 4. ]
              ActivityIndicator.Size Size.Large
              ActivityIndicator.Color "#ffb100" ]    

let view model =
    view [ ViewProperties.Style [ Flex 1. ] ] 
         [ listView model.items 
           statusView model.status ]