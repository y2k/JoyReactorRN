module JoyReactor.Tests.UseCasesTests

open Xunit
open Swensen.Unquote
open JoyReactor.Components

[<Fact>]
let ``open post test`` () =
    TestFramework.assertTest
    <| fun env ->
        FeedScreen.OpenPost 4444270
        |> TabsScreen.FeedMsg
        |> env.dispatch
        |> env.assetJson "9a7e041f" 2

[<Fact>]
let ``open tags test`` () =
    TestFramework.assertTest
    <| fun env ->
        TabsScreen.SelectPage 1
        |> env.dispatch
        |> env.assetJson "aed3391d" 2

        TagsScreen.OpenTag "purpleisaprose"
        |> TabsScreen.TagsMsg
        |> env.dispatch
        |> env.assetJson "db255b1d" 5

[<Fact>]
let ``feed test`` () =
    TestFramework.assertTest
    <| fun env ->
        env.initView |> env.assetJson "60c93f24" 5

        FeedScreen.LoadNextPage
        |> TabsScreen.FeedMsg
        |> env.dispatch
        |> env.assetJson "6e1ee9f0" 2
