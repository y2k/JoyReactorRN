namespace JoyReactor.Web

module ReactVirtualized =
    open Fable.Core
    open Fable.React
    open Fable.React.Props
    open Fable.Core.JsInterop

    type ReactVirtualizedProps =
        | RowRenderer of ({| index : int; key : string; style : string |} -> ReactElement)
        | RowCount of int
        | Width of float
        | Height of float
        | OverscanRowCount of int
        | RowHeight of ({| index : int |} -> float)
        | [<CompiledName("rowHeight")>] RowHeightFixed of float
        | OnScroll of ({| scrollTop : int |} -> unit)
        | ScrollTop of int option
        interface Props.IHTMLProp

    let inline autoSizer (elems : ReactElement list) : ReactElement =
        ofImport "AutoSizer" "react-virtualized/dist/commonjs/AutoSizer" (keyValueList CaseRules.LowerFirst []) elems

    let inline list (props : IHTMLProp list) (elems : ReactElement list) : ReactElement =
        ofImport "List" "react-virtualized/dist/commonjs/List" (keyValueList CaseRules.LowerFirst props) elems

module DebounceComponent =
    open Fable.Core
    open Fable.React

    type Props =
        { onValueChanged : int option -> unit
          render: (int -> unit) -> ReactElement
          model: obj }

    type State = { token: int; lastValue: int option }

    type DebounceView (props) as this =
        inherit Component<Props, State>(props)

        do this.setInitState { token = 0; lastValue = None }

        override _.shouldComponentUpdate(nextProps, _) =
            props.model <> nextProps.model

        override this.render () =
            let onTimer value =
                let nv = Some value
                if this.state.lastValue <> nv then
                    this.setState (fun s _ -> { s with lastValue = nv })
                    props.onValueChanged <| nv

            props.render (fun value ->
                if this.state.lastValue <> None then
                    this.setState (fun s _ -> { s with lastValue = None })
                    props.onValueChanged None

                JS.clearTimeout this.state.token
                let token = JS.setTimeout (fun _ -> onTimer value) 200

                this.setState (fun s _ -> { s with token = token }))

    let debounceView model onValueChanged render =
        ofType<DebounceView, _, _> { onValueChanged = onValueChanged; render = render; model = model } []
