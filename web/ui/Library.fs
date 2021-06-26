namespace JoyReactor.Web

module Impl =
    open System
    open Fable.Core
    open Fable.Core.JsInterop
    open Fetch.Types
    open JoyReactor.Types

    let downloadAndParse url =
        async {
            let baseUrl = Browser.Dom.document.location.origin

            let! r =
                Fetch.fetch
                    (sprintf "%s/parse/%s" baseUrl (JS.encodeURIComponent url))
                    [ Credentials RequestCredentials.Include ]
                |> Async.AwaitPromise

            return! r.json<ParseResponse> () |> Async.AwaitPromise
        }

    let postForm form =
        async {
            let textForm =
                sprintf "url=%s&form=%s" (Uri.EscapeDataString form.url) (Uri.EscapeDataString form.form)

            let baseUrl = Browser.Dom.document.location.origin

            let! r =
                Fetch.fetch
                    (sprintf "%s/form" baseUrl)
                    [ Method HttpMethod.POST
                      Credentials RequestCredentials.Include
                      Body !^textForm ]
                |> Async.AwaitPromise

            return! r.json<ParseResponse> () |> Async.AwaitPromise
        }

module Styles =
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core

    let theme =
        createMuiTheme [ Palette [ Primary [ Main orange.``800`` ] ] ]

    let appBar title =
        appBar [ AppBarProp.Position AppBarPosition.Fixed ] [
            toolbar [] [
                typography [ Style [ FlexGrow 1 ]
                             Variant TypographyVariant.H6
                             MaterialProp.Color ComponentColor.Inherit ] [
                    str title
                ]
                iconButton [ Style [ MarginRight -12 ]
                             MaterialProp.Color ComponentColor.Inherit ] [
                    icon [] [ str "more_vert" ]
                ]
            ]
        ]

module MessagesScreen =
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core
    open JoyReactor.Types
    open JoyReactor.Components.MessagesScreen

    let viewThread (thread: Message) dispatch =
        listItem [ Key(string thread.date) ] [
            listItemAvatar [] [
                avatar [ Src thread.userImage ] []
            ]
            listItemText [ ListItemTextProp.Primary <| str thread.userName
                           ListItemTextProp.Secondary <| str thread.text ] []
        ]

    let view model dispatch =
        list [] [
            for t in model.messages do
                yield viewThread t dispatch
        ]

module ThreadsScreen =
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core
    open JoyReactor.Types
    open JoyReactor.Components.ThreadsScreen

    let viewThread (thread: Message) dispatch =
        listItem [ Key(string thread.date)
                   ListItemProp.Button true
                   OnClick(fun _ -> dispatch @@ OpenThread thread) ] [
            listItemAvatar [] [
                avatar [ Src thread.userImage ] []
            ]
            listItemText [ ListItemTextProp.Primary <| str thread.userName
                           ListItemTextProp.Secondary <| str thread.text ] []
        ]

    let view model dispatch =
        match model.notAuthorized with
        | true ->
            button [ Style [ Width "-moz-available"
                             CSSProp.Position PositionOptions.Absolute
                             Left "16px"
                             Right "16px"
                             Bottom "50%" ]
                     ButtonProp.Variant ButtonVariant.Contained
                     MaterialProp.Color ComponentColor.Primary
                     OnClick @@ fun _ -> dispatch OpenAuthorization ] [
                str "Войти"
            ]
        | false ->
            list [] [
                for t in model.threads do
                    yield viewThread t dispatch
            ]

module FeedScreen =
    module I = JoyReactor.Image

    open JoyReactor.Types
    open JoyReactor.Components.FeedScreen
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core

    let private viewPost dispatch (post: Post) =
        card [ Style [ Flex 1. ] ] [
            cardHeader [ CardHeaderProp.Avatar
                         <| avatar [ Src post.userImage.url ] []
                         CardHeaderProp.Title <| str post.userName
                         CardHeaderProp.Subheader
                         <| str (sprintf "%O" post.created) ] []
            cardActionArea [ OnClick ^ fun _ -> dispatch ^ OpenPost post.id ] [
                if post.title = "" then
                    div [] []
                else
                    cardContent [] [
                        typography [ Variant TypographyVariant.Caption ] [
                            str post.title
                        ]
                    ]
                (match post.image with
                 | [| i |] ->
                     let (iurl, h) = I.urlWithHeight 400. i

                     cardMedia [ Image iurl
                                 Style [ Height h ] ]
                 | _ -> div [] [])
            ]
            cardActions [] [
                typography [] [
                    str <| sprintf "%g" post.rating
                ]
            ]
        ]

    let private viewItemList (model: Model) dispatch =
        let viewItemVirt
            (x: {| index: int
                   key: string
                   style: string |})
            =
            match model.items.[x.index] with
            | Actual i ->
                listItem [ Key ^ string i.id
                           HTMLAttr.Custom("style", x.style) ] [
                    viewPost dispatch i
                ]
            | Old i ->
                listItem [ Key ^ string i.id
                           HTMLAttr.Custom("style", x.style) ] [
                    viewPost dispatch i
                ]
            | LoadNextDivider ->
                listItem [ Key "divider"
                           HTMLAttr.Custom("style", x.style) ] [
                    button [ Style [ Flex 1. ]
                             ButtonProp.Variant ButtonVariant.Contained
                             MaterialProp.Color ComponentColor.Primary
                             OnClick @@ fun _ -> dispatch LoadNextPage ] [
                        str "Еще"
                    ]
                ]

        let listItemHeight (x: {| index: int |}) =
            match model.items.[x.index] with
            | Actual i
            | Old i ->
                if Array.isEmpty i.image then
                    112.0 + 10.0
                else
                    446.0 + 10.0
            | LoadNextDivider -> 36.0 + 10.0

        DebounceComponent.debounceView
            model
            (fun x -> dispatch <| EndScrollChange x)
            (fun d ->
                ReactVirtualized.autoSizer [ Style [ Flex "1 1 auto" ]
                                             ReactVirtualized.Children
                                                 (fun size ->
                                                     ReactVirtualized.list [ Key "posts-list"
                                                                             ReactVirtualized.OnScroll
                                                                                 (fun x -> d x.scrollTop)
                                                                             ReactVirtualized.ScrollTop model.scroll
                                                                             ReactVirtualized.Width
                                                                                 Browser.Dom.window.innerWidth
                                                                             ReactVirtualized.Height size.height
                                                                             ReactVirtualized.OverscanRowCount 1
                                                                             ReactVirtualized.RowCount(
                                                                                 Array.length model.items
                                                                             )
                                                                             ReactVirtualized.RowHeight listItemHeight
                                                                             ReactVirtualized.RowRenderer viewItemVirt ] []) ])


    let view (model: Model) dispatch =
        fragment [] [
            yield viewItemList model dispatch
            if model.hasNew then
                yield
                    button [ Style [ CSSProp.Position PositionOptions.Fixed
                                     Bottom 60
                                     Left 12
                                     Right 12
                                     Width "-moz-available" ]
                             ButtonProp.Variant ButtonVariant.Contained
                             MaterialProp.Color ComponentColor.Primary
                             OnClick @@ fun _ -> dispatch ApplyPreloaded ] [
                        str "Новые посты"
                    ]
            if model.loading then
                yield
                    div [ Style [ Top 70
                                  CSSProp.Position PositionOptions.Fixed
                                  Display DisplayOptions.Flex
                                  Width "100%"
                                  JustifyContent "center" ] ] [
                        circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ]
                    ]
            yield snackbar [ Open false; Message @@ str "Ошибка" ] []
        ]

module TagsScreen =
    open JoyReactor.Types
    open JoyReactor.Components.TagsScreen
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core

    let private viewTagList dispatch (model: Model) =
        let tags =
            Array.concat [ model.userTags
                           model.topTags ]

        let viewItemVirt
            (x: {| index: int
                   key: string
                   style: string |})
            =
            let tag = tags.[x.index]

            listItem [ HTMLAttr.Custom("style", x.style)
                       Key x.key
                       ListItemProp.Button true
                       OnClick(fun _ -> dispatch @@ OpenTag tag.name) ] [
                listItemAvatar [] [
                    avatar [ Src tag.image ] []
                ]
                listItemText [ ListItemTextProp.Primary ^ str tag.name ] []
            ]

        ReactVirtualized.list [ Key "tags-list"
                                ReactVirtualized.Width Browser.Dom.window.innerWidth
                                ReactVirtualized.Height(Browser.Dom.window.innerHeight - 118.0)
                                ReactVirtualized.RowCount(Array.length tags)
                                ReactVirtualized.RowHeightFixed 56.0
                                ReactVirtualized.RowRenderer viewItemVirt ] []

    let view (model: Model) dispatch =
        fragment [] [
            viewTagList dispatch model
            snackbar [ Open false; Message ^ str "Ошибка" ] []
        ]

module LoginScreen =
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core
    open JoyReactor.Components.LoginScreen

    let view model dispatch =
        div [ Style [ CSSProp.Padding "16px" ] ] [
            formControl [ MaterialProp.Margin FormControlMargin.Normal
                          Required true
                          FullWidth true ] [
                inputLabel [] [ str "Логин" ]
                input [ OnChange(fun e _ -> !!e?target?value |> UsernameMsg |> dispatch)
                        AutoComplete "email" ]
            ]

            formControl [ MaterialProp.Margin FormControlMargin.Normal
                          Required true
                          FullWidth true ] [
                inputLabel [] [ str "Пароль" ]
                input [ OnChange(fun e _ -> !!e?target?value |> PasswordMsg |> dispatch)
                        HTMLAttr.Type "password"
                        AutoComplete "current-password" ]
            ]

            button [ OnClick(fun _ -> dispatch LoginMsg)
                     HTMLAttr.Disabled(not model.isEnabled)
                     HTMLAttr.Type "submit"
                     FullWidth true
                     ButtonProp.Variant ButtonVariant.Contained
                     MaterialProp.Color ComponentColor.Primary ] [
                str "Войти"
            ]
        ]

module ProfileScreen =
    open JoyReactor.Types
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core
    open JoyReactor.Components.ProfileScreen

    let private viewStars count =
        typography [ TypographyProp.Color TypographyColor.Primary ] [
            String.init 10 (fun i -> if i < count then "★ " else "☆ ")
            |> str
        ]

    let profileView (profile: Profile) dispatch =
        div [ Style [ BackgroundColor "#f8f8f8"
                      PaddingTop "16px"
                      CSSProp.AlignItems AlignItemsOptions.Center
                      Display DisplayOptions.Flex
                      FlexDirection "column" ] ] [
            avatar [ Style [ Width "80px"
                             Height "80px"
                             MarginBottom "8px" ]
                     Src profile.userImage.url ] []
            typography [ Variant TypographyVariant.H5 ] [
                str profile.userName
            ]
            typography [ Variant TypographyVariant.Body1 ] [
                str <| sprintf "Рейтинг: %g" profile.rating
            ]
            divider [ Style [ AlignSelf AlignSelfOptions.Stretch
                              MarginTop "16px" ] ]
            div [ Style [ PaddingTop "16px"
                          PaddingBottom "16px"
                          BackgroundColor "white"
                          Display DisplayOptions.Flex
                          JustifyContent "center"
                          AlignSelf AlignSelfOptions.Stretch ] ] [
                viewStars profile.stars
            ]
            divider [ Style [ AlignSelf AlignSelfOptions.Stretch ] ]
            div [ Style [ PaddingTop "16px"
                          PaddingBottom "16px"
                          BackgroundColor "white"
                          AlignSelf AlignSelfOptions.Stretch ] ] [
                typography [ Style [ AlignSelf AlignSelfOptions.Stretch
                                     MarginLeft "16px"
                                     MarginRight "16px"
                                     MarginBottom "8px" ]
                             Variant TypographyVariant.Body1 ] [
                    str "Прогресс до следующей звезды:"
                ]
                linearProgress [ Style [ AlignSelf AlignSelfOptions.Stretch
                                         CSSProp.BorderRadius "4px"
                                         MarginLeft "16px"
                                         MarginRight "16px"
                                         Height "20px" ]
                                 LinearProgressProp.Variant LinearProgressVariant.Determinate
                                 Value(int profile.progressToNewStar) ]
            ]
            divider [ Style [ AlignSelf AlignSelfOptions.Stretch
                              MarginBottom "16px" ] ]
            button [ Style [ AlignSelf AlignSelfOptions.Stretch
                             MarginLeft "16px"
                             MarginRight "16px" ]
                     ButtonProp.Variant ButtonVariant.Contained
                     MaterialProp.Color ComponentColor.Primary
                     OnClick @@ fun _ -> dispatch Logout ] [
                str "Выйти"
            ]
        ]

    let view model dispatch =
        match model with
        | ProfileLoading -> div [] []
        | ProfileModel profile -> profileView profile dispatch
        | LoginModel model -> LoginScreen.view model (LoginMsg >> dispatch)

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
        | ThreadsModel m -> ThreadsScreen.view m (ThreadsMsg >> dispatch)
        | ProfileModel m -> ProfileScreen.view m (ProfileMsg >> dispatch)

    let view model dispatch =
        let viewTab title index =
            bottomNavigationAction [ Label
                                     ^ typography [ Variant TypographyVariant.Body2 ]
                                       ^ [ str title ]
                                     OnClick(fun _ -> dispatch ^ SelectPage index) ]

        let toIndex =
            function
            | FeedModel _ -> 0
            | TagsModel _ -> 1
            | ThreadsModel _ -> 2
            | ProfileModel _ -> 3

        fragment [] [
            contentView model dispatch
            appBar [ Style [ Bottom 0; Top "auto" ]
                     AppBarProp.Position AppBarPosition.Fixed ] [
                bottomNavigation [ ShowLabels true
                                   Value ^ toIndex model ] [
                    viewTab "Лента" 0
                    viewTab "Теги" 1
                    viewTab "Сообщения" 2
                    viewTab "Профиль" 3
                ]
            ]
        ]

module PostScreen =
    open JoyReactor.Types
    open JoyReactor.Components.PostScreen
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core

    let private viewTopComment (comments: Comment []) =
        let viewComment (comment: Comment) =
            listItem [ AlignItems ListItemAlignItems.FlexStart ] [
                listItemAvatar [] [
                    avatar [ Src comment.image.url ] []
                ]
                listItemText [ ListItemTextProp.Primary ^ str comment.userName
                               ListItemTextProp.Secondary(
                                   fragment [] [
                                       str comment.text
                                       typography [ Component("span" |> ReactElementType.ofHtmlElement)
                                                    MaterialProp.Color ComponentColor.Primary ] [
                                           str <| sprintf " (%g)" comment.rating
                                       ]
                                   ]
                               ) ] []
            ]

        comments |> Array.map viewComment |> list []

    let private contentView dispatch (model: Model) comments =
        div [ Style [ CSSProp.Padding "16px" ] ] [
            (match model.image with
             | Some i ->
                 cardMedia [ Image i.url
                             Style [ Height 0
                                     PaddingTop(sprintf "%f%%" (100. / i.aspect)) ] ]
             | _ -> div [] [])

            h3 [] [ str "Теги:" ]
            div
                []
                (model.tags
                 |> Array.map
                    ^ fun tag ->
                        chip [ Style [ CSSProp.Margin "2px" ]
                               Label ^ str tag
                               OnClick(fun _ -> dispatch <| OpenTag tag) ])

            h3 [] [ str "Лучшие комментари:" ]
            viewTopComment comments
        ]

    let view (model: Model) dispatch =
        fragment [] [
            if model.isLoaded then
                contentView dispatch model model.comments
            else
                div [] []
        ]

module ApplicationScreen =
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core
    open JoyReactor.Components.ApplicationScreen

    let private contentView model dispatch =
        match model.history with
        | (TabsModel m) :: _ -> TabsScreen.view m (TabsMsg >> dispatch)
        | (PostsModel m) :: _ -> FeedScreen.view m (PostsMsg >> dispatch)
        | (PostModel m) :: _ -> PostScreen.view m (PostMsg >> dispatch)
        | (MessagesModel m) :: _ -> MessagesScreen.view m (MessagesMsg >> dispatch)
        | [] -> failwithf "illegal state (%O)" model

    let view model dispatch =
        muiThemeProvider [ Theme(ProviderTheme.Theme Styles.theme) ] [
            Styles.appBar model.title
            div [ Style [ Height "100%"
                          PaddingTop 60
                          PaddingBottom 60 ] ] [
                contentView model dispatch
            ]
        ]

module Program =
    open Elmish
    open Browser.Dom

    module A = JoyReactor.Components.ApplicationScreen

    let withNavigation p =
        let sub _ =
            Cmd.ofSub
            @@ fun dispatch ->
                window.onpopstate <-
                    fun _ ->
                        history.pushState ("", null, null)
                        dispatch A.NavigateBack

        history.pushState ("", null, null)
        Program.withSubscription sub p

module App =
    open Elmish
    open Elmish.React
    open Elmish.Navigation
    open Elmish.HMR

    module D = JoyReactor.Components.ApplicationScreen
    module E = JoyReactor.Components.ElmishInterpretator

#if !DEBUG
    [<Fable.Core.Emit("require('offline-plugin/runtime').install();")>]
    let initOfflinePlugin () = Fable.Core.Util.jsNative

    Browser.Dom.window.addEventListener ("load", (fun _ -> initOfflinePlugin ()))
#endif

    let t =
        JoyReactor.SyncExecutor.init Impl.downloadAndParse Impl.postForm

    Program.mkProgram (E.wrapInit t D.init) (E.wrapUpdate t D.update) ApplicationScreen.view
    |> Program.withReactBatched "elmish-app"
    |> Program.withNavigation
#if DEBUG
    |> Program.withConsoleTrace
#endif
    |> Program.run
