module Tests

open Xunit
open JoyReactor.Domain
open JoyReactor.Types
open JoyReactor.Utils

let private def =
    { text = ""
      date = 0L
      isMine = false
      userName = ""
      userImage = "" }

[<Fact>]
let ``getLastOffsetOrDefault is valid`` () =
    Assert.Equal(0L, getLastOffsetOrDefault [||])
    getLastOffsetOrDefault 
        [| { def with date = 1L }
           { def with date = 100L }
           { def with date = 10L } |]
    |> curry Assert.Equal 100L