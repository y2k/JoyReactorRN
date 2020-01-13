module ThreadsScreen

open Elmish
open Fable.ReactNative.Helpers
open Fable.ReactNative.Props
open JoyReactor
open JoyReactor.Types
module UI = CommonUi
module S = Services
module R = JoyReactor.Services.EffRuntime
module D = JoyReactor.SyncDomain
type LocalDb = CofxStorage.LocalDb

type Model = { items : Message []; status : Result<unit, exn> option }

type Msg =
    | Refresh
    | RefreshComplete of Result<string option, exn>
    | ThreadsLoaded of Message []
    | ThreadSelected of string

let sub (db : LocalDb) = db.messages |> Seq.map (fun x -> x.Key) |> Domain.selectThreads |> ThreadsLoaded

let init : Model * Cmd<Msg> = { items = [||]; status = None }, Cmd.ofMsg Refresh

let update model msg =
    match msg with
    | ThreadsLoaded x -> { model with items = x }, Cmd.none
    | Refresh ->
        { model with status = None },
        D.messages None |> R.run |> Cmd.map RefreshComplete
    | RefreshComplete (Ok (Some next)) ->
        model,
        D.messages ^ Some next |> R.run |> Cmd.map RefreshComplete
    | RefreshComplete (Ok None) -> { model with status = Some <| Ok () }, Cmd.none
    | RefreshComplete (Error x) -> log x { model with status = Some <| Error x }, Cmd.none
    | _ -> model, Cmd.none

let private itemView dispatch i =
    touchableHighlight [ TouchableHighlightProperties.Style [ Margin $ 4. ]
                         TouchableHighlightProperties.ActiveOpacity 0.7
                         OnPress(dispatch <! ThreadSelected i.userName) ] [
        view [ ViewProperties.Style [ FlexDirection FlexDirection.Row; Padding $ 8. ] ] [
            image [ ImageProperties.Style [ Width $ 48.; Height $ 48.; BorderRadius 24.; MarginRight $ 8. ]
                    Source <| remoteImage [ Uri i.userImage ] ]
            view [ ViewProperties.Style [ Flex 1. ] ] [
                text [ TextProperties.Style [ FontWeight FontWeight.Bold
                                              TextStyle.Color "#404040"
                                              FontSize 15. ] ] i.userName
                text [ TextProperties.Style [ TextStyle.Color "#808080"; FontSize 15. ]
                       NumberOfLines 2. ] i.text
                text [ TextProperties.Style [ AlignSelf Alignment.FlexEnd; TextStyle.Color "#bdbdbd" ] ]
                     (longToTimeDelay i.date) ] ] ]

let view model dispatch =
    view [ ViewProperties.Style [ Flex 1. ] ] [
        UI.list model.items (itemView dispatch) (fun x -> x.userName) [ OnRefresh (dispatch <! Refresh); Refreshing false ]
        UI.loadingView <| Option.isNone model.status ]
