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

    JoyReactor.Components.ActionModule.downloadAndParseImpl <-
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
    module I = JoyReactor.Image
    open JoyReactor.Types
    open JoyReactor.Components.FeedScreen
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core

    let private viewPost dispatch (post : Post) = 
        card [ Style [ Flex 1. ] ] [
            if post.title = ""
                then div [] []
                else
                    cardContent [] [
                        typography [ Variant TypographyVariant.Subtitle1 ] [ 
                            str post.title ] ]
            (match post.image with
             | [| i |] ->
                 cardMedia [
                     Image @@ fst @@ I.urlWithHeight 400. i
                     Style [ Height 0; PaddingTop (sprintf "%f%%" (100. / i.aspect)) ] ]
             | _ -> div [] [])
            cardActions [] [
                button 
                    [ ButtonProp.Size ButtonSize.Small
                      OnClick ^ fun _ -> dispatch ^ OpenPost post ] [ 
                    str "Открыть" ] ] ]

    let private viewItemList (model : Model) dispatch =
        let viewItem dispatch (i : PostState) =
            match i with 
            | Actual i -> listItem [ Key ^ string i.id ] [ viewPost dispatch i ]
            | Old i -> listItem [ Key ^ string i.id ] [ viewPost dispatch i ]
            | LoadNextDivider ->
                listItem [ Key "divider" ] [
                    button 
                        [ Style [ Flex 1. ]
                          ButtonProp.Variant ButtonVariant.Contained
                          MaterialProp.Color ComponentColor.Primary
                          OnClick @@ fun _ -> dispatch LoadNextPage ] [ 
                        str "Еще" ] ]

        list [] (model.items |> Array.map (viewItem dispatch))

    let view (model : Model) dispatch =
        fragment [] [
            yield viewItemList model dispatch
            if model.hasNew then
                yield
                    button 
                        [ Style [ 
                              CSSProp.Position PositionOptions.Fixed
                              Bottom 60; Left 12; Right 12 ]
                          ButtonProp.Variant ButtonVariant.Contained
                          MaterialProp.Color ComponentColor.Primary
                          OnClick @@ fun _ -> dispatch ApplyPreloaded ] [ 
                        str "Новые посты" ]
            if model.loading then
                yield
                    div [
                        Style [
                            Top 70
                            CSSProp.Position PositionOptions.Fixed
                            Display DisplayOptions.Flex
                            Width "100%"
                            JustifyContent "center" ] ] [
                        circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
            yield snackbar [ Open false; Message @@ str "Ошибка" ] [] ]

module TagsScreen =
    open JoyReactor.Types
    open JoyReactor.Components.TagsScreen
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core

    let private viewTagList dispatch (comments : Tag []) =
        let viewItem (tag : Tag) =
            listItem
                [ ListItemProp.Button true
                  OnClick (fun _ -> dispatch @@ OpenTag tag) ] [
                listItemAvatar [] [
                    avatar [ Src tag.image ] [] ]
                listItemText 
                    [ ListItemTextProp.Primary ^ str tag.name ] [] ]

        comments
        |> Array.map viewItem
        |> list []

    let view (model : Model) dispatch =
        fragment [] [
            viewTagList dispatch model.tags
            snackbar [ Open false; Message ^ str "Error" ] [] ]

module LoginScreen =
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core
    open JoyReactor.Components.LoginScreen

    let view model dispatch =
        div [ Style [ CSSProp.Padding "16px" ] ] [
            formControl 
                [ MaterialProp.Margin FormControlMargin.Normal
                  HTMLAttr.Required true
                  MaterialProp.FullWidth true] [
                inputLabel [] [ str "Логин" ]
                input 
                    [ OnChange (fun e _ -> !!e?target?value |> UsernameMsg |> dispatch)
                      AutoComplete "email" ] ]

            formControl 
                [ MaterialProp.Margin FormControlMargin.Normal
                  HTMLAttr.Required true
                  MaterialProp.FullWidth true] [
                inputLabel [] [ str "Пароль" ]
                input 
                    [ OnChange (fun e _ -> !!e?target?value |> PasswordMsg |> dispatch)
                      HTMLAttr.Type "password"
                      AutoComplete "current-password" ] ]

            button 
                [ OnClick (fun _ -> dispatch LoginMsg)
                  HTMLAttr.Disabled (not model.isEnabled)
                  HTMLAttr.Type "submit"
                  MaterialProp.FullWidth true
                  ButtonProp.Variant ButtonVariant.Contained
                  MaterialProp.Color ComponentColor.Primary ]  [
                str "Войти" ] ]

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
        | ProfileModel m -> LoginScreen.view m (ProfileMsg >> dispatch)

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

    let private viweTopComment (comments : Comment []) =
        let viewComment (comment : Comment) =
            listItem [ AlignItems ListItemAlignItems.FlexStart ] [
                listItemAvatar [] [
                    avatar [ Src comment.image.url ] [] ]
                listItemText [ 
                    ListItemTextProp.Primary ^ str comment.userName
                    ListItemTextProp.Secondary ^ str comment.text ] [] ]

        comments
        |> Array.sortByDescending ^ fun comment -> comment.rating
        |> Array.take (min comments.Length 10)
        |> Array.map viewComment
        |> list []

    let private contentView dispatch (post : Post) =
        fragment [] [
            card [ Style [ Flex 1. ] ] [
                cardContent [] [
                    typography [ Variant TypographyVariant.H6 ] [ 
                        str post.title ] ]
                (match post.image with
                 | [| i |] ->
                     cardMedia [
                         Image i.url
                         Style [ Height 0; PaddingTop (sprintf "%f%%" (100. / i.aspect)) ] ]
                 | _ -> div [] [])
                cardActions [] [
                    button 
                        [ ButtonProp.Size ButtonSize.Small
                          OnClick ignore ] [
                        str "Open comments" ] ] ]

            h3 [] [ str "Теги:" ]
            div [] (
                post.tags 
                |> Array.map ^ fun tag -> 
                    chip [ Style [ CSSProp.Margin "2px" ]; Label ^ str tag ])

            h3 [] [ str "Лучшие комментари:" ]
            viweTopComment post.comments ]

    let view (model : Model) dispatch = 
        fragment [] [
            match model.post with
            | Some post -> contentView dispatch post
            | None -> div [] [] ]

module StackNavigationComponent =
    open Fable.React
    open Fable.React.Props
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
            div [ Style [ PaddingTop 60; PaddingBottom 60 ] ] [
                contentView model dispatch ] ]

open Elmish
open Elmish.React
open Elmish.Navigation
open Elmish.HMR
module D = JoyReactor.Components.StackNavigationComponent

Program.mkProgram D.init (flip D.update) StackNavigationComponent.view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
