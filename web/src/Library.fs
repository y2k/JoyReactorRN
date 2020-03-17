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

module Interpretator =
    open Fable.Core
    open JoyReactor.Types

    JoyReactor.Components.ActionModule.downloadAndParse <-
        fun url ->
            async {
                let! r = 
                    Fetch.fetch (sprintf "http://localhost:8090/parse/%s" (JS.encodeURIComponent url)) [] 
                    |> Async.AwaitPromise
                return! r.json<ParseResponse>() |> Async.AwaitPromise
            }

module Styles =
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core

    let theme = 
        createMuiTheme [
            Palette [
                Primary [ Main orange.``800`` ] ] ]

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

    let private viewItem dispatch (i : PostState) =
        match i with 
        | Actual i -> listItem [ Key ^ string i.id ] [ viewPost dispatch i ]
        | Old i -> listItem [ Key ^ string i.id ] [ viewPost dispatch i ]
        | LoadNextDivider ->
            listItem [ Key "divider" ] [
                button 
                    [ ButtonProp.Size ButtonSize.Small
                      OnClick ^ fun _ -> dispatch LoadNextPage ] [ 
                    str "Load next" ] ]

    let private viewItemList (model : Model) dispatch =
        list [] (model.items |> Array.map (viewItem dispatch))

    let view (model : Model) dispatch =
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
                | false -> viewItemList model dispatch
            yield 
                snackbar [ Open false; Message ^ str "Error" ] [] ]

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
                      OnClick (fun _ -> dispatch @@ OpenTag i) ] [ 
                    str "Open" ] ] ]

    let view (model : Model) (dispatch : Msg -> unit) =
        div [ Style [ PaddingTop 60; PaddingBottom 50 ] ] [
            yield
                match not model.loaded with
                | true ->
                    div [ Style [ Display DisplayOptions.Flex; JustifyContent "center" ] ] [
                        yield circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
                | false ->
                    list [] (model.tags |> Array.map (fun x -> listItem [ Key x.name ] [ viewItem dispatch x ]))
            yield 
                snackbar [ Open false; Message ^ str "Error" ] [] ]

module TabsScreen =
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core
    open JoyReactor.Components.TabsScreen

    let contentView model dispatch =
        match model with
        | FeedModel m -> FeedScreen.view m (FeedMsg >> dispatch)
        | TagsModel m -> TagsScreen.view m (TagsMsg >> dispatch)

    let view model dispatch =
        fragment [] [
            contentView model dispatch
            appBar 
                [ Style [ Bottom 0; Top "auto" ]
                  AppBarProp.Position AppBarPosition.Fixed ] [
                bottomNavigation [ ShowLabels true ] [
                    bottomNavigationAction [ Label ^ str "Feed"; OnClick (fun _ -> dispatch ^ SelectPage 0) ]
                    bottomNavigationAction [ Label ^ str "Tags"; OnClick (fun _ -> dispatch ^ SelectPage 1) ]
                    bottomNavigationAction [ Label ^ str "Messages"; OnClick (fun _ -> dispatch ^ SelectPage 2) ]
                    bottomNavigationAction [ Label ^ str "Profile"; OnClick (fun _ -> dispatch ^ SelectPage 3) ] ] ] ]

module PostScreen =
    open JoyReactor.Types
    open JoyReactor.Components.PostScreen
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core

    let contentView (post : Post) =
        div [] []

    let view (model : Model) msg = 
        fragment [] [
            match model.post with
            | Some post -> contentView post
            | None -> div [] [] ]

module StackNavigationComponent =
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core
    open JoyReactor.Components.StackNavigationComponent

    let private contentView model dispatch =
        match model.history with
        | (TabsModel m) :: _ -> TabsScreen.view m (TabsMsg >> dispatch)
        | (PostsModel m) :: _ -> FeedScreen.view m (PostsMsg >> dispatch)
        | (PostModel m) :: _ -> PostScreen.view m (PostMsg >> dispatch)
        | [] -> failwithf "illegal state (%O)" model

    let view model dispatch =
        muiThemeProvider [ Theme (ProviderTheme.Theme Styles.theme) ] [
            Styles.appBar "JR"
            contentView model dispatch ]

open Elmish
open Elmish.React
open Elmish.Navigation
open Elmish.HMR
module D = JoyReactor.Components.StackNavigationComponent

Program.mkProgram D.init (flip D.update) StackNavigationComponent.view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
