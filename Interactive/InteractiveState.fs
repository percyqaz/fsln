namespace FSLN

open FSLN.Operations

[<RequireQualifiedAccess>]
type Selection =
    | File of FileTreeFile
    | Folder of FileTreeFolder
    | Project of Project
    | Solution of Solution

    member this.ParentProject: Project option =
        match this with
        | File file -> Some file.ParentProject
        | Folder folder -> Some folder.ParentProject
        | Project project -> Some project
        | Solution _ -> None

    member this.FullPath: string =
        match this with
        | File file -> file.FullPath
        | Folder folder -> folder.FullPath
        | Project project -> project.FullPath
        | Solution solution -> solution.FullPath

    member this.ToParent() : Parent option =
        match this with
        | File file -> Some file.Parent
        | Folder folder -> Some(Parent.Folder(folder))
        | Project project -> Some(Parent.Project(project))
        | Solution _ -> None

type InteractiveState =
    {
        mutable Running: bool
        Solution: Solution
        mutable Expanded: Set<string>
        mutable Selected: Selection
        mutable Buffer: string
        mutable StatusLine: string
        Keymap: ResizeArray<string * string>
        mutable Theme: Theme
    }

    member this.IsExpanded(folder: FileTreeFolder) : bool = this.Expanded.Contains(folder.FullPath)

    member this.IsExpanded(project: Project) : bool =
        this.Expanded.Contains(project.FullPath)

    static member Create(solution: Solution) : InteractiveState =
        {
            Running = true
            Solution = solution
            Expanded = Set.empty
            Selected = Selection.Solution(solution)
            Buffer = ""
            StatusLine = ""
            Keymap = ResizeArray()
            Theme = Theme.Default
        }

    member this.Bind(string: string, target: string) : unit =
        let inline special (s: string) = s.Replace("<", "＜").Replace(">", "＞")
        this.Keymap.Insert(0, (special string, special target))

module InteractiveState =

    let private previous<'T> (siblings: ResizeArray<'T>, child: 'T) : 'T option =
        let index = siblings.IndexOf(child)
        if index > 0 then Some siblings.[index - 1] else None

    let private next<'T> (siblings: ResizeArray<'T>, child: 'T) : 'T option =
        let index = siblings.IndexOf(child)
        if index + 1 < siblings.Count then Some siblings.[index + 1] else None

    let rec bottom_child_tree (state: InteractiveState, entry: FileTreeEntry) : Selection =
        match entry with
        | File file -> Selection.File(file)
        | Folder folder ->
            if state.IsExpanded(folder) then
                bottom_child_tree(state, folder.Children.[folder.Children.Count - 1])
            else
                Selection.Folder(folder)

    let bottom_child_project (state: InteractiveState, project: Project) : Selection =
        if state.IsExpanded(project) then
            bottom_child_tree(state, project.Children.[project.Children.Count - 1])
        else
            Selection.Project(project)

    [<TailCall>]
    let rec find_next_in_tree (state: InteractiveState, current: FileTreeEntry) : Selection option =
        match next(current.Parent.Children, current) with
        | Some(File file_below) -> Some(Selection.File(file_below))
        | Some(Folder folder_below) -> Some(Selection.Folder(folder_below))
        | None ->
            match current.Parent with
            | Parent.Project project ->
                match next(state.Solution.Projects, project) with
                | Some project_below -> Some(Selection.Project(project_below))
                | None -> None
            | Parent.Folder folder -> find_next_in_tree(state, Folder folder)

    let navigate_up (state: InteractiveState) : Selection =
        match state.Selected with
        | Selection.Solution _ -> state.Selected
        | Selection.Project project ->
            match previous(state.Solution.Projects, project) with
            | Some project_above -> bottom_child_project(state, project_above)
            | None -> Selection.Solution(state.Solution)
        | Selection.Folder folder ->
            match previous(folder.Parent.Children, Folder folder) with
            | Some entry_above -> bottom_child_tree(state, entry_above)
            | None ->
                match folder.Parent with
                | Parent.Folder parent -> Selection.Folder(parent)
                | Parent.Project project -> Selection.Project(project)
        | Selection.File file ->
            match previous(file.Parent.Children, File file) with
            | Some entry_above -> bottom_child_tree(state, entry_above)
            | None ->
                match file.Parent with
                | Parent.Folder parent_folder -> Selection.Folder(parent_folder)
                | Parent.Project parent_project -> Selection.Project(parent_project)

    let navigate_down (state: InteractiveState) : Selection =
        match state.Selected with
        | Selection.Solution solution -> Selection.Project(solution.Projects.[0])
        | Selection.Project project ->
            if state.IsExpanded(project) then
                match project.Children.[0] with
                | File child_file -> Selection.File(child_file)
                | Folder child_folder -> Selection.Folder(child_folder)
            else
                match next(state.Solution.Projects, project) with
                | Some project_below -> Selection.Project(project_below)
                | None -> Selection.Project(project)
        | Selection.Folder folder ->
            if state.IsExpanded(folder) then
                match folder.Children.[0] with
                | File child_file -> Selection.File(child_file)
                | Folder child_folder -> Selection.Folder(child_folder)
            else
                find_next_in_tree(state, Folder folder) |> Option.defaultValue state.Selected
        | Selection.File file -> find_next_in_tree(state, File file) |> Option.defaultValue state.Selected

    let navigate_out (state: InteractiveState) : Selection =
        match state.Selected with
        | Selection.Solution solution -> Selection.Solution(solution)
        | Selection.Project _ -> Selection.Solution(state.Solution)
        | Selection.Folder folder ->
            match folder.Parent with
            | Parent.Folder parent_folder -> Selection.Folder(parent_folder)
            | Parent.Project parent_project -> Selection.Project(parent_project)
        | Selection.File file ->
            match file.Parent with
            | Parent.Folder parent_folder -> Selection.Folder(parent_folder)
            | Parent.Project parent_project -> Selection.Project(parent_project)

    let expand_selected (state: InteractiveState) : unit =
        match state.Selected with
        | Selection.Solution _ -> ()
        | Selection.Project project -> state.Expanded <- state.Expanded.Add(project.FullPath)
        | Selection.Folder folder -> state.Expanded <- state.Expanded.Add(folder.FullPath)
        | Selection.File _ -> ()

    let rec collapse_selected (state: InteractiveState) : unit =
        match state.Selected with
        | Selection.Solution _ -> ()
        | Selection.Project project ->
            state.Expanded <- state.Expanded.Remove(project.FullPath)

            for subfolder in project.EnumerateSubfolders() do
                state.Expanded <- state.Expanded.Remove(subfolder.FullPath)
        | Selection.Folder folder ->
            if state.IsExpanded(folder) then
                state.Expanded <- state.Expanded.Remove(folder.FullPath)

                for subfolder in folder.EnumerateSubfolders() do
                    state.Expanded <- state.Expanded.Remove(subfolder.FullPath)
            else
                state.Selected <- navigate_out(state)
                collapse_selected(state)
        | Selection.File _ ->
            state.Selected <- navigate_out(state)
            collapse_selected(state)

    let move_selection_up (state: InteractiveState) : unit =
        match state.Selected with
        | Selection.Solution _ -> ()
        | Selection.Project _ -> ()
        | Selection.Folder folder -> move_folder_up(folder.ParentProject, folder)
        | Selection.File file -> move_file_up(file.ParentProject, file)

    let move_selection_down (state: InteractiveState) : unit =
        match state.Selected with
        | Selection.Solution _ -> ()
        | Selection.Project _ -> ()
        | Selection.Folder folder -> move_folder_down(folder.ParentProject, folder)
        | Selection.File file -> move_file_down(file.ParentProject, file)
