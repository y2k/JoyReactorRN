module JoyReactor.Tests.UseCasesTests

open Xunit
open Swensen.Unquote
open JoyReactor.Components

[<Fact>]
let ``open post test`` () =
    TestFramework.assertTest <| fun env ->
        FeedScreen.OpenPost 4444270
        |> TabsScreen.FeedMsg
        |> env.dispatch
        |> env.assetJson "9a7e041f" 2

[<Fact>]
let ``open tags test`` () =
    TestFramework.assertTest <| fun env ->
        let actual = TabsScreen.SelectPage 1 |> env.dispatch
        let expected = env.getJson "aed3391d"
        test <@ expected = actual @>

        let actual = TagsScreen.OpenTag "purpleisaprose" |> TabsScreen.TagsMsg  |> env.dispatch
        let expected = env.getJson "db255b1d"
        test <@ expected = actual @>

[<Fact>]
let ``feed test`` () =
    TestFramework.assertTest <| fun env ->
        let expected = env.getJson "60c93f24"
        test <@ expected = env.initView @>

        let actual = FeedScreen.LoadNextPage |> TabsScreen.FeedMsg |> env.dispatch
        let expected = env.getJson "6e1ee9f0"
        test <@ expected = actual @>
