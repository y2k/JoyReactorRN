module MaterialUiResearch

[<AutoOpen>]
module Prelude =
    let inline (^) f x = f x
    let inline (@@) f x = f x
    let inline (<!) f a () = f a
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f = async.Bind(ma, f >> async.Return)
    let inline flip f a b = f b a
    let wrap fmodel fmsg (a, b) = a |> fmodel, b |> Elmish.Cmd.map fmsg

module Styles =
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core

    let appBar title =
        appBar [ AppBarProp.Position AppBarPosition.Fixed ] [
            toolbar [] [
                typography 
                    [ Style [ FlexGrow 1 ]
                      Variant TypographyVariant.H6
                      MaterialProp.Color ComponentColor.Inherit ] [ 
                    str title ]
                iconButton 
                    [ Style [ MarginRight -12 ]
                      MaterialProp.Color ComponentColor.Inherit ] [ 
                    icon [] [ str "more_vert" ] ] ] ]

module FeedScreen =
    open JoyReactor.Types
    open JoyReactor.Components.FeedScreen
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core

    let private viewPost dispatch (i : Post) = 
        card [ Style [ Flex 1. ] ] [
            (match i.image with
             | [| i |] ->
                 cardMedia [
                     Image i.url
                     Style [ Height 0; PaddingTop (sprintf "%f%%" (100. / i.aspect)) ] ]
             | _ -> div [] [])
            cardContent [] [
                typography [ Variant TypographyVariant.H6 ] [ 
                    str i.title ]
                typography [ Variant TypographyVariant.Subtitle1 ] [ 
                    str i.title ] ]
            cardActions [] [
                button 
                    [ ButtonProp.Size ButtonSize.Small
                      OnClick ^ fun _ -> dispatch ^ OpenPost i ] [ 
                    str "Open post" ] ] ]

    let viewItem dispatch (i : PostState) =
        match i with 
        | Actual i -> viewPost dispatch i
        | Old i -> viewPost dispatch i
        | LoadNextDivider ->
            button 
                [ ButtonProp.Size ButtonSize.Small
                  OnClick ^ fun _ -> dispatch LoadNextPage ] [ 
                str "Load next" ]

    let contentView (model : Model) dispatch =
        div [ Style [ PaddingTop 60; PaddingBottom 50 ] ] [
            yield
                (match model.hasNew with
                 | true -> 
                    button 
                        [ ButtonProp.Size ButtonSize.Small
                          OnClick ^ fun _ -> dispatch ApplyPreloaded ] [ 
                        str "New posts" ]
                 | false -> div [] [])
            yield
                match model.loading with
                | true ->
                    div [ Style [ Display DisplayOptions.Flex; JustifyContent "center" ] ] [
                        yield circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
                | false ->
                    list [] [ yield! model.items |> Array.map (fun x -> listItem [] [ viewItem dispatch x ]) ]
            yield 
                snackbar [ Open false; Message ^ str "Error" ] [] ]

    let view model dispatch =
        fragment [] [
            Styles.appBar "Posts"
            contentView model dispatch
            appBar 
                [ Style [ Bottom 0; Top "auto" ]
                  AppBarProp.Position AppBarPosition.Fixed ] [
                bottomNavigation [ ShowLabels true ] [
                    bottomNavigationAction [ Label ^ str "Feed" ] 
                    bottomNavigationAction [ Label ^ str "Tags" ] 
                    bottomNavigationAction [ Label ^ str "Messages" ] 
                    bottomNavigationAction [ Label ^ str "Profile" ] ] ] ]

module TagsScreen =
    open JoyReactor.Types
    open JoyReactor.Components.TagsScreen
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core

    let private viewItem dispatch (i : Tag) = 
        card [ Style [ Flex 1. ] ] [
            cardMedia [
                Image i.image
                Style [ Height 0; PaddingTop "100%" ] ]
            cardContent [] [
                typography [ Variant TypographyVariant.H6 ] [ 
                    str i.name ]
                typography [ Variant TypographyVariant.Subtitle1 ] [ 
                    str i.name ] ]
            cardActions [] [
                button 
                    [ ButtonProp.Size ButtonSize.Small
                      OnClick ignore ] [ 
                    str "Open tag" ] ] ]

    let view (model : Model) dispatch =
        div [ Style [ PaddingTop 60; PaddingBottom 50 ] ] [
            yield
                match not model.loaded with
                | true ->
                    div [ Style [ Display DisplayOptions.Flex; JustifyContent "center" ] ] [
                        yield circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
                | false ->
                    list [] [ yield! model.tags |> Array.map (fun x -> listItem [] [ viewItem dispatch x ]) ]
            yield 
                snackbar [ Open false; Message ^ str "Error" ] [] ]

module Interpretator =
    open Fable.Core
    open JoyReactor
    open JoyReactor.Types
    open JoyReactor.CofxStorage
    open Elmish
    type 'a Action = 'a JoyReactor.Components.Action

    let private db = ref LocalDb.empty

    let private downloadPostsForUrl url =
        async {
            let! r = 
                Fetch.fetch (sprintf "http://localhost:8090/parse/%s" (JS.encodeURIComponent url)) [] 
                |> Async.AwaitPromise
            let! pr = r.json<ParseResponse>() |> Async.AwaitPromise
            db := DomainInterpetator.saveAllParseResults !db pr
        }

    let private invoke furl callback : _ Async =
        async {
            let (ldb, opUrl) = furl !db
            db := ldb
            match opUrl with None -> () | Some url -> do! downloadPostsForUrl url
            let (ldb, result) = callback !db
            db := ldb
            return result
        }

    let private toCmd (action : 'msg Action list) : 'msg Cmd =
        action
        |> List.map @@ fun action ->
            match action with
            | Action.Eff (url, callback) ->
                Cmd.OfAsync.perform (fun () -> invoke url callback) () id
        |> Cmd.batch

    let init (f : 'arg -> 'model * 'msg Action list) : ('arg -> 'model * 'msg Cmd) =
        fun arg ->
            let (model, action) = f arg
            model, toCmd action

    let udpate (f : 'model -> 'msg -> 'model * 'msg Action list) : ('msg -> 'model -> 'model * 'msg Cmd) =
        fun msg model ->
            let (model, action) = f model msg
            model, toCmd action

open Elmish
open Elmish.React
open Elmish.Navigation
open Elmish.HMR
module D = JoyReactor.Components.FeedScreen

Program.mkProgram (Interpretator.init D.init) (Interpretator.udpate D.update) FeedScreen.view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.runWith JoyReactor.Types.FeedSource
