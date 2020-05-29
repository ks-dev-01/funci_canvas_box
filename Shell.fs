namespace FuncuiCanvasBoxes

module Shell =
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
    open System

    let version = 0.1

    let mutable MaxBoxId = 0L

    type BoxComponent = {
        Id : int64
        Name : string
        Point : Point
    }

    type State =
        { BoxList : BoxComponent list
          CurrentBox : BoxComponent 
          CurrentName : string }

    type Msg =
        | NewBox of Point
        | BoxClicked of int64
        | CurrentNameChanged of string
    
    let init : State * Cmd<Msg> =
        let newBox = { Id = -1L 
                       Name = ("")
                       Point = Point(10.,10.) }

        { BoxList = []
          CurrentBox = newBox
          CurrentName = "" }, Cmd.none

    let newBox point =  MaxBoxId <- MaxBoxId + 1L
                        {
                          Id = MaxBoxId
                          Name = sprintf "New Box...%i" MaxBoxId
                          Point = point
                        }

    let update (msg: Msg) (state: State): State * Cmd<_> =
        match msg with
        | NewBox point -> let newBoxT = newBox point
                          let boxes = state.BoxList @ [newBoxT]
                          { state with BoxList = boxes }, Cmd.none
        | BoxClicked id ->
                            printfn "BoxClicked Id: %i" id
                            if id <> state.CurrentBox.Id then
                                printfn "New Box"
                                let oldBox = state.CurrentBox
                                let clicked = (state.BoxList |> List.filter (fun x -> x.Id = id)).Head
                                let newList = (state.BoxList |> List.filter (fun x -> x.Id <> id)) @ (if oldBox.Id = -1L then [] else [oldBox])
                                printfn "BoxClicked Object: %A" clicked
                                printfn "BoxClicked clicked.Name: %s" clicked.Name
                                { state with CurrentBox = clicked;BoxList = newList;CurrentName=clicked.Name }, Cmd.none
                            else
                              printfn "Same Box"
                              state, Cmd.none
        | CurrentNameChanged text ->
                          if state.CurrentBox.Id = -1L then
                            state,Cmd.none
                          else
                              printfn "---------------------CurrentQueryNameChanged Start--------------------"
                              printfn "Text: %s" text
                              let newCurrent = { state.CurrentBox with Name = text }
                              printfn "New CurrentBox : %A" newCurrent
                              printfn "---------------------CurrentQueryNameChanged End--------------------"
                              { state with CurrentBox = newCurrent },Cmd.none

    let view (state: State) (dispatch) =

        Grid.create [
            Grid.columnDefinitions "0.6*,0.4*"
            Grid.showGridLines true
            Grid.children [
                Grid.create [
                    Grid.column 0
                    Grid.showGridLines true
                    Grid.children [
                        Canvas.create [
                            Canvas.name "MainCanvas"
                            Canvas.background "white"
                            Canvas.onPointerPressed (fun event -> let eventSourceName = (event.Source :?> Control).Name
                                                                  printfn "Canvas Event %A, Point %A, Source %A, Name %A" event (event.GetPosition (event.Source :?> IVisual)) event.Source eventSourceName
                                                                  if eventSourceName = "MainCanvas" then
                                                                    dispatch (NewBox (event.GetPosition (event.Source :?> IVisual)))
                                                                    event.Handled <- true                                                                          
                                                                  )
                            Canvas.children [
                                printfn "--------Begin Drawing Query Boxes---------"
                                printfn "CurrentBox: %i" state.CurrentBox.Id
                                printfn "CurrentBox: %s" state.CurrentName
                                if state.CurrentBox.Id <> -1L then
                                    yield Rectangle.create [
                                            Rectangle.name (sprintf "R%i" state.CurrentBox.Id)
                                            Rectangle.fill "white"
                                            Rectangle.stroke "black"
                                            Rectangle.strokeThickness 1.
                                            Rectangle.width 80.0
                                            Rectangle.height 25.0
                                            Rectangle.left state.CurrentBox.Point.X
                                            Rectangle.top state.CurrentBox.Point.Y
                                            Rectangle.onPointerPressed (fun event -> let eventSourceName = (event.Source :?> Control).Name
                                                                                     printfn "Rect Event %A, Point %A, Source %A, Name %A" event (event.GetPosition (event.Source :?> IVisual)) event.Source eventSourceName
                                                                                     let xid =
                                                                                        match eventSourceName with
                                                                                        | x when x <> null -> Int64.Parse(x.Replace("R", ""))
                                                                                        | _ -> -1L
                                                                                     event.Handled <- true
                                                                                     dispatch (BoxClicked xid))
                                    ]

                                    yield TextBlock.create [
                                            TextBlock.name (sprintf "T%i" state.CurrentBox.Id)
                                            TextBlock.text (sprintf "%s" state.CurrentBox.Name)
                                            TextBlock.width 80.0
                                            TextBlock.height 25.0
                                            TextBlock.left (state.CurrentBox.Point.X + 5.)
                                            TextBlock.top (state.CurrentBox.Point.Y + 5.)
                                            TextBlock.onPointerPressed (fun event -> let eventSourceName = (event.Source :?> Control).Name
                                                                                     printfn "TextBox Event %A, Point %A, Source %A, Name %A" event (event.GetPosition (event.Source :?> IVisual)) event.Source eventSourceName
                                                                                     let xid =
                                                                                        match eventSourceName with
                                                                                        | x when x <> null -> Int64.Parse(x.Replace("T", ""))
                                                                                        | _ -> -1L
                                                                                     event.Handled <- true
                                                                                     dispatch (BoxClicked xid))
                                    ]

                                for nbox in state.BoxList do
                                    printfn "Rectangle Query Box: %A" nbox
                                    
                                    yield Rectangle.create [
                                            Rectangle.name (sprintf "R%i" nbox.Id)
                                            Rectangle.fill "white"
                                            Rectangle.stroke "black"
                                            Rectangle.strokeThickness 1.
                                            Rectangle.width 80.0
                                            Rectangle.height 25.0
                                            Rectangle.left nbox.Point.X
                                            Rectangle.top nbox.Point.Y
                                            Rectangle.onPointerPressed (fun event -> let eventSourceName = (event.Source :?> Control).Name
                                                                                     printfn "Rect Event %A, Point %A, Source %A, Name %A" event (event.GetPosition (event.Source :?> IVisual)) event.Source eventSourceName
                                                                                     let xid =
                                                                                        match eventSourceName with
                                                                                        | x when x <> null -> Int64.Parse(x.Replace("R", ""))
                                                                                        | _ -> -1L

                                                                                     event.Handled <- true
                                                                                     dispatch (BoxClicked xid))
                                        ]

                                    yield TextBlock.create [
                                            TextBlock.name (sprintf "T%i" nbox.Id)
                                            TextBlock.text (sprintf "%s" nbox.Name)
                                            TextBlock.width 80.0
                                            TextBlock.height 25.0
                                            TextBlock.left (nbox.Point.X + 5.)
                                            TextBlock.top (nbox.Point.Y + 5.)
                                            TextBlock.onPointerPressed (fun event -> let eventSourceName = (event.Source :?> Control).Name
                                                                                     printfn "TextBox Event %A, Point %A, Source %A, Name %A" event (event.GetPosition (event.Source :?> IVisual)) event.Source eventSourceName
                                                                                     let xid =
                                                                                        match eventSourceName with
                                                                                        | x when x <> null -> Int64.Parse(x.Replace("T", ""))
                                                                                        | _ -> -1L
                                                                                     event.Handled <- true
                                                                                     dispatch (BoxClicked xid))
                                        ]
                                        
                                printfn "--------End Drawing Query Boxes---------"   
                            ]
                        ]
                    ]
                ]

                StackPanel.create [
                    Grid.column 1
                    StackPanel.children [
                        
                        TextBox.create [ TextBox.text state.CurrentName
                                         TextBox.onTextChanged (fun text -> printfn "Text %s" text
                                                                            dispatch (CurrentNameChanged text)) ]
                    ]
                ]
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

            Elmish.Program.mkProgram (fun () -> init) update view
            |> Program.withHost this
            |> Program.run
