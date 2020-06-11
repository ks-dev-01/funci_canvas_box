namespace FuncuiCanvasBoxes

module Shell =
    open System
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Elmish
    open Avalonia.Input
    open Avalonia.Layout
    open Avalonia
    open Avalonia.FuncUI.Components

    type Pages = 
    | PageOther
    | PageSelector

    type PagesState = 
    | PageOtherState of PageOther.State
    | PageSelectorState of PageSelectorPage.State

    type State =
        /// you can use optional types to represent the existance of a value
        /// this gives you type safety and prevents you from doing akward ifs
        { CurrentPage : Pages 
          PageOtherState : PageOther.State option
          PageSelectorState : PageSelectorPage.State option }

    type Msg =
        | ChangePage of Pages
        | PageOtherMsg of PageOther.Msg
        | PageSelectorMsg of PageSelectorPage.Msg
    
    let init : State * Cmd<Msg> =
//        { CurrentPage = PageSelector; PageOtherState = None; PageSelectorState = Some PageSelectorPage.init }, Cmd.none
        { CurrentPage = PageOther; PageOtherState = Some PageOther.init; PageSelectorState = Some PageSelectorPage.init }, Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<_> =
        match msg with
        | ChangePage page ->
            match page with
            | PageOther ->
                match state.PageOtherState with
                | Some pageState -> { state with CurrentPage = page }, Cmd.none
                | None -> { state with PageOtherState = Some PageOther.init; CurrentPage = page }, Cmd.none
            | PageSelector -> 
                match state.PageSelectorState with
                | Some pageState -> { state with CurrentPage = page }, Cmd.none
                | None -> { state with PageSelectorState = Some PageSelectorPage.init; CurrentPage = page }, Cmd.none
        | PageOtherMsg subMsg ->
            match state.PageOtherState with
            | Some pageState ->
                let updatedState, _ = PageOther.update subMsg pageState
                { state with PageOtherState = Some updatedState }, Cmd.none
            | None -> state, Cmd.none
        | PageSelectorMsg subMsg ->
            match state.PageSelectorState with
            | Some pageState ->
                let updatedState, _ = PageSelectorPage.update subMsg pageState
                { state with PageSelectorState = Some updatedState }, Cmd.none
            | None -> state, Cmd.none

    let private buttonPanel gridPos (state: State) (dispatch: Msg -> unit) =
        StackPanel.create [
            gridPos
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                Button.create [
                    Button.content "PageOther"
                    Button.onClick (fun _ -> dispatch (ChangePage PageOther))
                ]
                Button.create [
                    Button.content "PageSelector"
                    Button.onClick (fun _ -> dispatch (ChangePage PageSelector))
                ]
            ]
        ]

    let view (state: State) (dispatch: Msg -> unit) =
        Grid.create [
            Grid.rowDefinitions "0.1*, 0.9*"
            Grid.showGridLines true
            Grid.children [
                buttonPanel (Grid.row 0) state dispatch
                Grid.create [
                    Grid.row 1
                    Grid.children [
                        match state.CurrentPage with
                        | PageOther ->
                            match state.PageOtherState with
                            | Some pageState -> 
                                PageOther.view pageState (PageOtherMsg >> dispatch) 
                            | None -> ()
                        | PageSelector ->
                            match state.PageSelectorState with
                            | Some pageState -> 
                                PageSelectorPage.view pageState (PageSelectorMsg >> dispatch) 
                            | None -> ()
                    ]
                ]
            ]
        ]
        
        //ExampleSubPage.view state.SubPageState (ExampleSubPageMsg >> dispatch)
        
    type MainWindow() as this =
        inherit HostWindow()
        do
            base.Title <- "FuncuiCanvasBoxes A"
            base.Width <- 800.0
            base.Height <- 600.0
            base.MinWidth <- 800.0
            base.MinHeight <- 600.0

            //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
            //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true
            // you can use the following DEBUG helpers to trace elmish updates and to 
            // open Avalonia DevTools
#if DEBUG
            this.AttachDevTools(KeyGesture.Parse("F12"))
#endif
            Elmish.Program.mkProgram (fun () -> init) update view
            |> Program.withHost this
#if DEBUG
            |> Program.withConsoleTrace
#endif
            |> Program.run
