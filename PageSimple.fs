namespace FuncuiCanvasBoxes

module PageSimple =
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

    type State =
        { Text : string }

    type Msg =
        | OnTextChanged of string
    
    let init : State =
        { Text = "" }

    let update (msg: Msg) (state: State): State * Cmd<_> =
        match msg with
        | OnTextChanged text ->
            { state with Text = text }, Cmd.none

    let view (state: State) (dispatch: Msg -> unit) =
        let otherText = "Another Text View" + state.Text
        Grid.create [
            Grid.columnDefinitions "0.4*, 0.6*"
            Grid.showGridLines true
            Grid.children [
                TextBox.create [ 
                    Grid.column 0
                    TextBox.text state.Text 
                    TextBox.onTextChanged (fun text -> dispatch (OnTextChanged text))
                    TextBox.watermark "Enter here"
                    ]
                TextBlock.create [ 
                    Grid.column 1
                    TextBlock.text otherText
                    ]         
            ]
        ]