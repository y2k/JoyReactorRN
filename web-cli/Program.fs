module Downloader =
    open System
    open System.Net
    open System.Net.Http

    let private gerResultCookie (handler : HttpClientHandler) url =
        handler.CookieContainer.GetCookies (Uri url)
        |> Seq.map ^ fun x -> x.Name, x.Value
        |> Seq.toList

    let loadHtml (url : string) (cookies : (string * string) list) : (string * (string * string) list) Async = 
        async {
            use handler = new HttpClientHandler()
            handler.CookieContainer <- CookieContainer()
            cookies
            |> List.iter ^ fun (k, v) -> handler.CookieContainer.Add(Uri url, Cookie(k, v))
            use httpClient = new HttpClient(handler)
            let! response = httpClient.GetStringAsync url |> Async.AwaitTask
            let responseCookie = gerResultCookie handler url
            return response, responseCookie
        }

module Domain =
    open System.Text.Json
    open System.Text.Json.Serialization
    open Suave
    open Suave.Cookie
    open JoyReactor
    open JoyReactor.Types

    let private options = JsonSerializerOptions()
    options.Converters.Add(JsonFSharpConverter())

    let private wrapToOption isEmpty xs =
        match isEmpty xs with
        | true -> None
        | false -> Some xs

    let private extractCookiese (ctx : HttpContext) =
        ctx.request.cookies
        |> Map.toList
        |> List.map ^ fun (k, v) -> k, v.value

    let private mkResponseBody html =
        { posts = Parsers.parsePostsWithNext html |> wrapToOption (fun x -> Array.isEmpty x.posts)
          userName = Parsers.parseUserName html
          userTags = Parsers.readUserTags html |> wrapToOption Array.isEmpty
          topTags = Parsers.parseTopTags html |> wrapToOption Array.isEmpty
          post = Parsers.parsePost html }
        |> fun response -> JsonSerializer.SerializeToUtf8Bytes (response, options)

    let parse url (ctx : HttpContext) =
        async {
            let! (html, cookies) = 
                extractCookiese ctx
                |> Downloader.loadHtml url

            let bodyWebPart =
                mkResponseBody html
                |> Successful.ok

            let cookieWebPart =
                cookies
                |> List.map ^ fun (k, v) -> HttpCookie.createKV k v
                |> List.map setCookie
                |> List.reduce compose

            return! (compose bodyWebPart cookieWebPart) ctx
        }

    type PostForm =
        { url : string
          form : string
          csrfName : string }

    let private tryParseForm (ctx : HttpContext) : PostForm option =
        failwith "???"

    let private sendFormTo url form cookies : (string * (string * string) list) Async =
        failwith "???"

    let sendForm (ctx : HttpContext) =
        async {
            match tryParseForm ctx with
            | None -> return! RequestErrors.BAD_REQUEST "Invalid json request" ctx
            | Some form ->
                let! (html, cookies) = 
                    extractCookiese ctx
                    |> Downloader.loadHtml form.url

                let token = Parsers.getCsrfToken form.csrfName html
                let form' = sprintf "%s&%s=%s" form.form form.csrfName token

                let! (html, cookies) = sendFormTo form.url form' cookies

                let bodyWebPart =
                    mkResponseBody html
                    |> Successful.ok

                let cookieWebPart =
                    cookies
                    |> List.map ^ fun (k, v) -> HttpCookie.createKV k v
                    |> List.map setCookie
                    |> List.reduce compose

                return! (compose bodyWebPart cookieWebPart) ctx
        }

open Suave
open Suave.Operators
open Suave.Filters

[<EntryPoint>]
let main _ =
    choose [
        GET >=> pathScan "/parse/%s" Domain.parse 
            >=> Writers.setMimeType "application/json"
        POST >=> path "/form" >=> Domain.sendForm 
             >=> Writers.setMimeType "application/json"
        GET >=> path "/info" 
            >=> Successful.OK(sprintf "JR Parser (Suave) - %O" System.DateTime.Now) ]
    >=> Writers.setHeader "Access-Control-Allow-Origin" "*"
    |> startWebServer {
        defaultConfig with
            bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8090us ] }
    0
