namespace FuncuiCanvasBoxes

module PageSelectorPage =
    open System
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Elmish
    open Avalonia.Input
    open Avalonia
    open Avalonia.FuncUI.Components

    type SubPageState =
    | SimpleState of PageSimple.State
    | OtherState of PageOther.State
    | CanvasState of PageCanvas.State

    type SubPage = {
        Id : int
        SubPageState : SubPageState
    }

    let createSubPage id state = { Id = id; SubPageState = state }
    
    let createSimplePage id = 
        createSubPage id (SimpleState PageSimple.init )

    let createOtherPage id = 
        createSubPage id (OtherState PageOther.init )

    let createCanvasPage id = 
        createSubPage id (CanvasState PageCanvas.init )

    type SubPageMsg =
    | SimplePageMsg of PageSimple.Msg
    | OtherPageMsg of PageOther.Msg
    | CanvasPageMsg of PageCanvas.Msg
    
    type State =
        /// you can use optional types to represent the existance of a value
        /// this gives you type safety and prevents you from doing akward ifs
        { CurrentSubPageId : int
          SubPageList : SubPage list }

    type Msg =
        | NewPageSimple
        | NewPageOther
        | NewPageCanvas
        | ChangePage of int
        | SimplePageMsg of PageSimple.Msg
        | OtherPageMsg of PageOther.Msg
        | CanvasPageMsg of PageCanvas.Msg
    
    let init : State =
        let initSubPageList = 
            [ createOtherPage 1
              createSimplePage 2
              createCanvasPage 3
              createCanvasPage 4
              createOtherPage 5 ]

        { CurrentSubPageId = 1; SubPageList = initSubPageList }

    let getCurrentPageOption state = state.SubPageList |> List.tryFind (fun x -> x.Id = state.CurrentSubPageId)

    let newSubpage subPageState state =
        let newId = (state.SubPageList |> List.maxBy (fun x -> x.Id)).Id + 1
        let newSubPage = {
            Id = newId
            SubPageState = subPageState
        }

        let updateList = newSubPage :: state.SubPageList
        { state with SubPageList = updateList }

    let updateSubPageState page msg =
        match page.SubPageState, msg with
        | SimpleState state, SimplePageMsg msg ->
            let updatedState,_ = PageSimple.update msg state
            { page with SubPageState = SimpleState updatedState } 
        | OtherState state, OtherPageMsg msg ->
            let updatedState,_ = PageOther.update msg state
            { page with SubPageState = OtherState updatedState }             
        | CanvasState state, CanvasPageMsg msg ->
            let updatedState,_ = PageCanvas.update msg state
            { page with SubPageState = CanvasState updatedState } 
        | _ ,_ -> page    

    let update (msg: Msg) (state: State): State * Cmd<_> =
        match msg with
        | NewPageSimple ->
            let updatedState = newSubpage (SimpleState PageSimple.init) state
            updatedState, Cmd.none
        | NewPageOther ->
            let updatedState = newSubpage (OtherState PageOther.init) state
            updatedState, Cmd.none
        | NewPageCanvas ->
            let updatedState = newSubpage (CanvasState PageCanvas.init) state
            updatedState, Cmd.none
        | ChangePage id ->
            { state with CurrentSubPageId = id }, Cmd.none
        | SimplePageMsg subMsg ->
            match getCurrentPageOption state with
            | Some page ->
                let newPage = updateSubPageState page msg
                let updatedList = state.SubPageList |> List.map (fun x -> if x.Id = newPage.Id then newPage else x)

                { state with SubPageList = updatedList }, Cmd.none
            | None -> state, Cmd.none    
        | OtherPageMsg subMsg ->
            match getCurrentPageOption state with
            | Some page ->
                let newPage = updateSubPageState page msg
                let updatedList = state.SubPageList |> List.map (fun x -> if x.Id = newPage.Id then newPage else x)

                { state with SubPageList = updatedList }, Cmd.none
            | None -> state, Cmd.none    
        | CanvasPageMsg subMsg ->
            match getCurrentPageOption state with
            | Some page ->
                let newPage = updateSubPageState page msg
                let updatedList = state.SubPageList |> List.map (fun x -> if x.Id = newPage.Id then newPage else x)

                { state with SubPageList = updatedList }, Cmd.none
            | None -> state, Cmd.none    

    let getNodeType node = 
        match node with
        | SimpleState -> "Simple Node"
        | OtherState -> "Other Node"
        | CanvasState -> "Canvas Node"

    let view (state: State) (dispatch: Msg -> unit) =
        Grid.create [
            Grid.columnDefinitions "0.4*, 0.6*"
            Grid.showGridLines true
            Grid.children [
                ListBox.create  [
                    Grid.column 0
                    ListBox.onSelectedItemChanged (fun obj -> 
                        if obj <> null then
                            let id,text = obj :?> int*string
                            dispatch (ChangePage id) 
                            )
                    ListBox.dataItems (state.SubPageList |> List.map (fun x -> (x.Id, sprintf "%s: %i" (getNodeType x.SubPageState) (x.Id))))
                    ListBox.itemTemplate (
                        DataTemplateView<int*string>.create (fun (id, text) ->
                            TextBlock.create [ TextBlock.text text ]
                        )
                    )
                ]
                Grid.create [
                    Grid.column 1
                    Grid.children [
                        let maybeCurrentPage = state.SubPageList |> List.tryFind (fun x -> x.Id = state.CurrentSubPageId)
                        match maybeCurrentPage with
                        | Some currentPage ->
                            match currentPage.SubPageState with
                            | SimpleState pageSimpleState ->
                                PageSimple.view pageSimpleState (SimplePageMsg >> dispatch)
                            | CanvasState pageCanvasState ->
                                PageCanvas.view pageCanvasState (CanvasPageMsg >> dispatch)    
                            | OtherState pageOtherState ->
                                PageOther.view pageOtherState (OtherPageMsg >> dispatch)    
                        | None -> ()
                    ]
                ]
            ]
        ]
