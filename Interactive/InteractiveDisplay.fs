namespace FSLN

open System
open System.Drawing
open System.Runtime.CompilerServices
open System.Text

type AnsiStringExtensions =

    [<Extension>]
    static member ForeColor(text: string, foreground: Color) : string =
        sprintf "\u001b[38;2;%d;%d;%dm%s\u001b[39m" foreground.R foreground.G foreground.B text

    [<Extension>]
    static member ForeColor(text: string, foreground: int) : string =
        text.ForeColor(Color.FromArgb(foreground))

    [<Extension>]
    static member BackColor(text: string, background: Color) : string =
        sprintf "\u001b[48;2;%d;%d;%dm%s\u001b[49m" background.R background.G background.B text

    [<Extension>]
    static member BackColor(text: string, background: int) : string =
        text.BackColor(Color.FromArgb(background))

    [<Extension>]
    static member Bold(text: string) : string = sprintf "\u001b[1m%s\u001b[22m" text

    [<Extension>]
    static member ClearRestOfLine(text: string) : string = sprintf "%s\u001b[K" text

type ScreenBuffer(height: int) =

    let lines = ResizeArray()
    let mutable cursor = 0
    let mutable scroll_position = 0

    member val ScrollOff = 6
    member val LinesBelow = 1
    member val Height = height

    member this.CursorHere() : unit = cursor <- lines.Count

    member this.Line(line: string) : unit = lines.Add(line)

    member this.Line(line: string, cursor_here: bool) : unit =
        if cursor_here then
            this.CursorHere()

        this.Line(line)

    member this.Draw() : unit =
        let sb = StringBuilder().Append("\u001b[H")

        let top_of_requested_view = max 0 (cursor - this.ScrollOff)

        if top_of_requested_view < scroll_position then
            scroll_position <- top_of_requested_view

        let bottom_of_requested_view =
            min (lines.Count - 1 + this.LinesBelow) (cursor + this.ScrollOff)

        if bottom_of_requested_view - this.Height + 1 > scroll_position then
            scroll_position <- bottom_of_requested_view - this.Height + 1

        let mutable index = scroll_position

        for i = 1 to this.Height do
            let line = if index < lines.Count then lines.[index] else ""
            sb.AppendLine(line.ClearRestOfLine()) |> ignore
            index <- index + 1

        Console.Write(sb.ToString())

        lines.Clear()

type InteractiveDisplay(state: InteractiveState) =

    let view = ScreenBuffer(Console.BufferHeight - 2)

    member inline private this.RenderFile
        (indent: string, icolor: Color, is_selected: bool, is_last: bool, file: FileTreeFile)
        : unit =
        let tree_marker =
            if is_last then state.Theme.TreeConnectors.Leaf else state.Theme.TreeConnectors.Branch

        let indent = indent + tree_marker.ForeColor(icolor)

        let line =
            sprintf "%c %s  " state.Theme.IconFile (file.Name.ForeColor(state.Theme.ColorFile))

        view.Line(indent + (if is_selected then line.BackColor(state.Theme.ColorSelection) else line), is_selected)

    member inline private this.RenderFolder
        (indent: string, icolor: Color, is_selected: bool, is_expanded: bool, is_last: bool, folder: FileTreeFolder)
        : unit =
        let tree_marker =
            if is_last then state.Theme.TreeConnectors.Leaf else state.Theme.TreeConnectors.Branch

        let indent = indent + tree_marker.ForeColor(icolor)

        let expand_marker =
            if is_expanded then state.Theme.ExpandMarkers.Open else state.Theme.ExpandMarkers.Closed

        let line =
            sprintf
                "%c %s %s"
                state.Theme.IconFolder
                ((folder.Name + "/").ForeColor(state.Theme.ColorFolder).Bold())
                (expand_marker.ToString().ForeColor(state.Theme.ColorExpandIcon))

        view.Line(indent + (if is_selected then line.BackColor(state.Theme.ColorSelection) else line), is_selected)

    member inline private this.RenderProject(is_selected: bool, is_expanded: bool, project: Project) : unit =
        let expand_marker =
            if is_expanded then state.Theme.ExpandMarkers.Open else state.Theme.ExpandMarkers.Closed

        let line =
            sprintf
                "%c %s %s"
                state.Theme.IconProject
                (project.Name.ForeColor(state.Theme.ColorProject).Bold())
                (expand_marker.ToString().ForeColor(state.Theme.ColorExpandIcon))

        view.Line((if is_selected then line.BackColor(state.Theme.ColorSelection) else line), is_selected)

    member inline private this.RenderSolution(solution: Solution) : unit =
        let is_selected = state.Selected = Selection.Solution(solution)

        let line =
            sprintf "%c %s " state.Theme.IconSolution (solution.Name.ForeColor(state.Theme.ColorSolution).Bold())

        view.Line((if is_selected then line.BackColor(state.Theme.ColorSelection) else line), is_selected)

    member this.RenderTree() : unit =

        let rec display_entry (indent: string, icolor: Color, is_last: bool, entry: FileTreeEntry) : unit =
            match entry with
            | File file ->
                let is_selected = state.Selected = Selection.File(file)
                this.RenderFile(indent, icolor, is_selected, is_last, file)
            | Folder folder ->
                let is_selected = state.Selected = Selection.Folder(folder)
                let is_expanded = state.IsExpanded(folder)
                this.RenderFolder(indent, icolor, is_selected, is_expanded, is_last, folder)

                if is_expanded then
                    let inner_color =
                        if is_selected then state.Theme.ColorConnectorsFolder else state.Theme.ColorConnectorsDefault

                    let mutable i = 0

                    while i < folder.Children.Count do
                        display_entry(
                            indent
                            + (if is_last then
                                   state.Theme.TreeConnectors.Empty
                               else
                                   state.Theme.TreeConnectors.Vertical.ForeColor(icolor)),
                            inner_color,
                            i + 1 = folder.Children.Count,
                            folder.Children.[i]
                        )

                        i <- i + 1

        let inline display_project (project: Project) : unit =
            let is_selected = state.Selected = Selection.Project(project)
            let is_expanded = state.IsExpanded(project)
            this.RenderProject(is_selected, is_expanded, project)

            if is_expanded then
                let icolor =
                    if is_selected then state.Theme.ColorConnectorsProject else state.Theme.ColorConnectorsDefault

                let mutable i = 0

                while i < project.Children.Count do
                    display_entry("", icolor, i + 1 = project.Children.Count, project.Children.[i])
                    i <- i + 1

        this.RenderSolution(state.Solution)

        for project in state.Solution.Projects do
            display_project(project)

    member this.Redraw() : unit =
        this.RenderTree()
        view.Draw()
        Console.WriteLine(state.StatusLine.ClearRestOfLine())
        Console.Write(state.Buffer.ForeColor(0x88FF88).Bold().ClearRestOfLine())
