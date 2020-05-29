namespace FuncuiCanvasBoxes

module Shell =
    open System
    open Elmish
    open Avalonia.Controls
    open Avalonia.Controls.Shapes
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Elmish
    open Avalonia.Layout
    open Avalonia.Input
    open Avalonia.Media
    open Avalonia
    open Avalonia.VisualTree

    let version = 0.1

    type BoxComponent = 
        { Id : int64
          Name : string
          Point : Point }

    type State =
        /// you can use optional types to represent the existance of a value
        /// this gives you type safety and prevents you from doing akward ifs
        { BoxList : BoxComponent list
          CurrentBox : BoxComponent option
          LastBoxId: int64 option
          CurrentName : string }

    type Msg =
        | NewBox of Point
        | BoxClicked of int64
        | CurrentNameChanged of string
    
    let init : State * Cmd<Msg> =
        /// here's where the optional values come at play
        /// in the beginning there's nothing so no need to initialize with 
        /// "unfinished" states like an empty box or -1
        { BoxList = []
          CurrentBox = None
          LastBoxId = None
          CurrentName = "" }, Cmd.none

    let newBox id point =  
        { Id = id
          Name = sprintf "New Box...%i" id
          Point = point }

    let update (msg: Msg) (state: State): State * Cmd<_> =
        match msg with
        | NewBox point ->
            let id =
                /// pattern matching is your friend it will help you take the correct road
                match state.LastBoxId with
                /// if there's a previous value it means we can just increment the value
                | Some id -> id + 1L
                /// if there's no value it means this is the first id
                | None -> 1L
            let newBoxT = newBox id point
            /// I choose to use the "::" union operator but you can keep using concat
            /// if it works better for you
            let boxes = newBoxT :: state.BoxList
            /// update the boxes and the id
            { state with BoxList = boxes; LastBoxId = Some id }, Cmd.none
        | BoxClicked id ->
            printfn "BoxClicked Id: %i" id
            /// we can use a tuple to return both values from the same place
            let (clicked, currentName) = 
                /// check with pattern matching if we have a current box
                match state.CurrentBox with 
                /// if we have a box and the id matches then return the box, else don't return anything
                | Some bx -> if bx.Id = id then (Some bx, bx.Name) else (None, "")
                | None -> 
                    /// if we don't have anything check if the id exists in the boxes
                    /// then return that box with its name
                    let bx = state.BoxList |> List.find(fun bx -> bx.Id =  id)
                    (Some bx, bx.Name)
            /// update the CurrentBpx (BoxComponent option) and the CurrentName
            { state with CurrentBox = clicked; CurrentName = currentName }, Cmd.none
        | CurrentNameChanged text ->
            match state.CurrentBox with
            /// again check if we have a box selected
            | Some bx ->
                /// update the box with the new Name
                let updated = { bx with Name = text }
                /// update the box list with the updated box (so it shows on the canvas list with its name updated)
                let boxes = 
                    state.BoxList 
                    |> List.map(fun cbx -> if cbx.Id = bx.Id then updated else cbx)
                { state with CurrentBox = Some updated; BoxList = boxes }, Cmd.none
            | None ->
                state, Cmd.none

    /// don't hesitate to create mini functions that display a specific part
    /// of your view, this will allow you to reduce visual nesting and localize things
    let private rightPanel state dispatch =
        StackPanel.create [
            Grid.column 1
            StackPanel.children [
                TextBox.create [ 
                    TextBox.text state.CurrentName
                    TextBox.onTextChanged (
                        fun text -> 
                            printfn "Text %s" text
                            dispatch (CurrentNameChanged text)
                    ) 
                ]
            ]
        ]

    let private label nbox = 
        TextBlock.create [
            TextBlock.tag nbox.Id
            TextBlock.text (sprintf "%s" nbox.Name)
            TextBlock.width 80.0
            TextBlock.height 25.0
            TextBlock.left (nbox.Point.X + 5.)
            TextBlock.top (nbox.Point.Y + 5.)
        ]  

    let private rectangle nbox =
        Rectangle.create [
            Rectangle.tag nbox.Id
            Rectangle.fill "white"
            Rectangle.stroke "black"
            Rectangle.strokeThickness 1.
            Rectangle.width 80.0
            Rectangle.height 25.0
            Rectangle.left nbox.Point.X
            Rectangle.top nbox.Point.Y
        ]

    let private canvasGrid state dispatch =
        Grid.create [
            Grid.column 0
            Grid.showGridLines true
            Grid.children [
                Canvas.create [
                    Canvas.name "MainCanvas"
                    Canvas.background "white"
                    Canvas.onPointerPressed (
                        fun event ->
                            event.Handled <- true                                                                       
                            let source = (event.Source :?> Control)
                            let eventSourceName = source.Name
                            printfn "Canvas Event %A, Point %A, Source %A, Name %A" event (event.GetPosition (source :> IVisual)) source eventSourceName
                            /// since the canvas and its children are in the same area its very likely that the source of the event
                            /// is either the canvas, the textblock or the rectangle, you can use Pattern matching once again
                            /// to check against the type of the source and decide which message to dispatch
                            match source with 
                            | :? Canvas ->
                                dispatch (NewBox (event.GetPosition (source :> IVisual)))
                            | :? TextBlock | :? Rectangle ->
                                dispatch (BoxClicked (source.Tag :?> int64))
                            | unknown ->
                                printfn "Unrecognized Element %A" unknown

                    )
                    Canvas.children [
                        printfn "--------Begin Drawing Query Boxes---------"
                        for nbox in state.BoxList do
                            printfn "Rectangle Query Box: %A" nbox
                            rectangle nbox
                            label nbox
                        printfn "--------End Drawing Query Boxes---------"   
                    ]
                ]
            ]
        ]

    let view (state: State) (dispatch: Msg -> unit) =
        Grid.create [
            Grid.columnDefinitions "0.6*,0.4*"
            Grid.showGridLines true
            Grid.children [
                canvasGrid state dispatch
                rightPanel state dispatch
            ]
        ]

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
