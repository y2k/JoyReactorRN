module JoyReactor.Free

open Fable.PowerPack.Fetch.Fetch_types

type FaceInstruction<'a> =
    | Fetch of (string * RequestProperties list * (string -> 'a))
    | ReadLine of (unit * (string -> 'a))
    | WriteLine of (string * (unit -> 'a))

let private mapI f =
    function 
    | ReadLine(x, next) -> ReadLine(x, next >> f)
    | WriteLine(x, next) -> WriteLine(x, next >> f)
    | Fetch(url, props, next) -> Fetch(url, props, next >> f)

type FaceProgram<'a> =
    | Free of FaceInstruction<FaceProgram<'a>>
    | Pure of 'a

let rec bind f =
    function 
    | Free x -> 
        x
        |> mapI (bind f)
        |> Free
    | Pure x -> f x

type FaceBuilder() =
    member this.Bind(x, f) = bind f x
    member this.Return x = Pure x
    member this.ReturnFrom x = x
    member this.Zero() = Pure()

let face = FaceBuilder()
let readLine() = Free(ReadLine((), Pure))
let writeLine text = Free(WriteLine(text, Pure))
let fetch url props = Free(Fetch(url, props, Pure))

let rec interpret =
    function 
    | Pure x -> x
    | Free(ReadLine(x, next)) -> 
        x
        |> (fun _ -> failwith "TODO")
        |> next
        |> interpret
    | Free(WriteLine(x, next)) -> 
        x
        |> (fun _ -> failwith "TODO")
        |> next
        |> interpret
    | Free(_) -> failwith "Not Implemented"

module Service =

    let getMyName =
        face {
            let! page = fetch "http://joyreactor.cc/donate" []
            return Domain.extractName page
        }

    let loadMyTags =
        face {
            let! name = getMyName
            name
            |> Option.map UrlBuilder.user
            ()
        }

    let login username password =
        face { 
            let! page = fetch "http://joyreactor.cc/ads" []
            let csrf = Domain.getCsrfToken page
            let r = Requests.login username password (csrf.Value)
            let! _ = r ||> fetch
            ()
        }
