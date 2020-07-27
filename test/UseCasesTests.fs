module JoyReactor.Tests.UseCasesTests

open Xunit
open Swensen.Unquote
open Elmish
open JoyReactor.Components
module D = ApplicationScreen
type R = System.Text.RegularExpressions.Regex

[<Fact>]
let ``first open test`` () =
    async {
        let viewRef = ref ""
        Program.mkProgram D.init (flip D.update) (fun model _ -> viewRef := model.ToString())
        |> Program.withConsoleTrace
        |> Program.run

        let view = R.Replace(!viewRef, "\s+", " ")
        test <@ view = "{ history = [TabsModel (FeedModel { source = FeedSource items = [||] hasNew = false loading = false })] }" @>

        do! Async.Sleep 3_000

        let view = R.Replace(!viewRef, "\s+", " ")
        test <@ view = "{ history = [TabsModel (FeedModel { source = FeedSource items = [||] hasNew = false loading = true })] }" @>
    } |> Async.RunSynchronously
