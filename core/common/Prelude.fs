[<AutoOpen>]
module Predule

let inline (^) f x = f x
let inline always a _ = a

module Async =
    let inline map f a = async.Bind(a, f >> async.Return)
