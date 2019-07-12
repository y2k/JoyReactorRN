module Tests

open JoyReactor.Domain
open JoyReactor.Types
open JoyReactor.CommonUtils
open Xunit

let private def =
    { text = ""
      date = 0.
      isMine = false
      userName = ""
      userImage = "" }

[<Fact>]
let ``selectMessageForUser should success``() =
    let actual =
        [| { def with userName = "1"; date = 1. }
           { def with userName = "2"; date = 110. }
           { def with userName = "1"; date = 100. }
           { def with userName = "3"; date = 90. }
           { def with userName = "1"; date = 10. } |]
        |> selectMessageForUser "1"
    let expected =
        [| { def with userName = "1"; date = 100. }
           { def with userName = "1"; date = 10. }
           { def with userName = "1"; date = 1. } |]
    Assert.Equal(expected |> Array.toList, actual)

[<Fact>]
let ``getLastOffsetOrDefault should success``() =
    Assert.Equal(0., getLastOffsetOrDefault [||])
    getLastOffsetOrDefault
        [| { def with date = 1. }
           { def with date = 100. }
           { def with date = 10. } |]
    |> curry Assert.Equal 100.

[<Fact>]
let ``selectThreads should success``() =
    let actual =
        [| { def with userName = "1"; date = 1. }
           { def with userName = "2"; date = 110. }
           { def with userName = "1"; date = 100. }
           { def with userName = "3"; date = 90. }
           { def with userName = "1"; date = 10. } |]
        |> selectThreads
    let expected =
        [| { def with userName = "2"; date = 110. }
           { def with userName = "1"; date = 100. }
           { def with userName = "3"; date = 90. } |]
    Assert.Equal(expected |> Array.toList, actual)
