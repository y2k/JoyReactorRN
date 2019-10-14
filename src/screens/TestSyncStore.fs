module TestSyncStore

open Elmish

module UrlBuilder = JoyReactor.UrlBuilder
module S = JoyReactor.Services
module D = JoyReactor.SyncStore
type LocalDb = JoyReactor.CofxStorage.LocalDb
type Log = JoyReactor.Log  

type Model = { db : LocalDb Option; isBusy : bool }
type Msg = SyncMsg | SyncComplete | LocalDbMsg of JoyReactor.CofxStorage.LocalDb

let init _ = { db = None; isBusy = false }, Cmd.none

let private id = 4111388

let downloadPost =
    S.ApiRequests.downloadString (UrlBuilder.post id) []
    >>= fun html -> 
        Log.log ^ sprintf "1.2"
        D.dispatch (fun db -> { db with parseRequests = Set.union (Set.ofSeq [html]) db.parseRequests })
    |> Async.Catch
    >>- fun result -> 
        Log.log ^ sprintf "%O" result
        SyncComplete

let update msg (model : Model) =
    match msg with
    | SyncMsg -> { model with isBusy = true }, downloadPost |> Cmd.OfAsync.result
    | SyncComplete -> {model with isBusy = false }, Cmd.none
    | LocalDbMsg db -> { model with db = Some db }, Cmd.none

let subscribe _ = JoyReactor.SyncStore.sub |> Cmd.map LocalDbMsg

open Fable.ReactNative.Helpers
open Fable.ReactNative.Props

let view (model : Model) dispatch =
    view [] [
        
        text [] "---"
        text [] (model.db |> Option.map (fun db -> db.posts) |> sprintf "%O")
        text [] "---"
        
        button [ 
            ButtonProperties.Disabled (model.isBusy)
            ButtonProperties.OnPress(fun _ -> dispatch SyncMsg) 
            ButtonProperties.Title "Reload" ] [] ]
