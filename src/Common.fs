namespace JoyReactor

type Log() =
    static member log (message : string,
                       [<System.Runtime.CompilerServices.CallerFilePath;
                         System.Runtime.InteropServices.Optional;
                         System.Runtime.InteropServices.DefaultParameterValue("")>] file : string,
                       [<System.Runtime.CompilerServices.CallerLineNumber;
                         System.Runtime.InteropServices.Optional;
                         System.Runtime.InteropServices.DefaultParameterValue(0)>] line : int) =
        printfn "LOG %s:%i :: %s" file line message

module Cmd =
    open Elmish
    let ofEffect f p =
        Cmd.OfAsyncImmediate.either (fun () -> p) () (Result.Ok >> f) (Result.Error >> f)
    let ofFiber f p = Cmd.OfAsync.either (fun () -> p) () f raise
    let ofEffect0 p = Cmd.ofSub (fun _ -> p |> Async.StartImmediate)

[<AutoOpen>]
module Utils =
    open System

    let inline ($) f d = f (Fable.ReactNative.Helpers.dip d)

    let longToTimeDelay _ = "2 часа"
    let mutable private startTime = DateTime.Now.Ticks / 10_000L

    let log msg x =
        let delay = DateTime.Now.Ticks / 10_000L - startTime
        printfn "LOGX (%O) :: %O" delay msg
        startTime <- DateTime.Now.Ticks / 10_000L
        x

module String =
    let toUpper (x : string) = x.ToUpper()

module Image =
    open Fable.Core.JS
    open Types

    let normalize url (w : float) (h : float) =
        sprintf "http://rc.y2k.work:8080/cache/fit?width=%i&height=%i&bgColor=ffffff&quality=75&url=%s" (int w) (int h)
            (encodeURIComponent url)

    let urlWithHeight limitWidth (attachment : Attachment) =
        let aspect = max 1.2 attachment.aspect
        let w = limitWidth
        let h = w / aspect
        normalize attachment.url w h, h

module Platform =
    open Fable.Core
    open Fable.ReactNative

    let openUrl url = async {
        do! RN.Linking.openURL url |> Async.AwaitPromise |> Async.Ignore }
