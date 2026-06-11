namespace fsln

open System

module Interactive =
    
    [<RequireQualifiedAccess>]
    type Selection =
        | File of FileTreeFile
        | Folder of FileTreeFolder
        | Project of Project
        | Solution of Solution
    
    type State =
        {
            mutable Running: bool
            Solution: Solution
            mutable Expanded: Set<string>
            mutable Selected: Selection
        }
        member this.IsExpanded(folder: FileTreeFolder) : bool = this.Expanded.Contains folder.FullPath
        member this.IsExpanded(project: Project) : bool = this.Expanded.Contains project.FullPath
        static member Create(solution: Solution) : State = { Running = true; Solution = solution; Expanded = Set.empty; Selected = Selection.Solution solution }
        
    let private previous<'t>(siblings: ResizeArray<'t>, child: 't) : 't option =
        let index = siblings.IndexOf(child)
        if index > 0 then Some siblings.[index - 1] else None
        
    let private next<'t>(siblings: ResizeArray<'t>, child: 't) : 't option =
        let index = siblings.IndexOf(child)
        if index + 1 < siblings.Count then Some siblings.[index + 1] else None
        
    let rec bottom_child_tree(state: State, entry: FileTreeEntry) : Selection =
        match entry with
        | File file -> Selection.File file
        | Folder folder ->
            if state.IsExpanded folder then
                bottom_child_tree(state, folder.Children.[folder.Children.Count - 1])
            else
                Selection.Folder folder
        
    let bottom_child_project(state: State, project: Project) : Selection =
        if state.IsExpanded project then
            bottom_child_tree(state, project.Children.[project.Children.Count - 1])
        else
            Selection.Project project
            
    let rec find_next_in_tree(state: State, current: FileTreeEntry) : Selection =
        match next(current.Parent.Children, current) with
        | Some (File file_below) -> Selection.File file_below
        | Some (Folder folder_below) -> Selection.Folder folder_below
        | None ->
            match current.Parent with
            | Parent.Project project -> 
                match next(state.Solution.Projects, project) with
                | Some project_below -> Selection.Project project_below
                // todo: None/Some pattern instead of returning current selection here, so this function can be used
                // todo: rn this function is only correct when `current` is what's selected
                | None -> state.Selected
            | Parent.Folder folder ->
                find_next_in_tree(state, Folder folder)
            
    let navigate_up(state: State) : Selection =
        match state.Selected with
        | Selection.Solution _ -> state.Selected
        | Selection.Project project ->
            match previous(state.Solution.Projects, project) with
            | Some project_above -> bottom_child_project(state, project_above)
            | None -> Selection.Solution state.Solution
        | Selection.Folder folder ->
            match previous(folder.Parent.Children, Folder folder) with
            | Some entry_above -> bottom_child_tree(state, entry_above)
            | None ->
                match folder.Parent with
                | Parent.Folder parent -> Selection.Folder parent
                | Parent.Project project -> Selection.Project project
        | Selection.File file ->
            match previous(file.Parent.Children, File file) with
            | Some entry_above -> bottom_child_tree(state, entry_above)
            | None ->
                match file.Parent with
                | Parent.Folder parent_folder -> Selection.Folder parent_folder
                | Parent.Project parent_project -> Selection.Project parent_project
            
    let navigate_down(state: State) : Selection =
        match state.Selected with
        | Selection.Solution solution -> Selection.Project solution.Projects.[0]
        | Selection.Project project ->
            if state.IsExpanded project then
                match project.Children.[0] with
                | File child_file -> Selection.File child_file
                | Folder child_folder -> Selection.Folder child_folder
            else
                match next(state.Solution.Projects, project) with
                | Some project_below -> Selection.Project project_below
                | None -> Selection.Project project
        | Selection.Folder folder ->
            if state.IsExpanded folder then
                match folder.Children.[0] with
                | File child_file -> Selection.File child_file
                | Folder child_folder -> Selection.Folder child_folder
            else
                find_next_in_tree(state, Folder folder)
        | Selection.File file -> find_next_in_tree(state, File file)
        
    let navigate_out(state: State) : Selection =
        match state.Selected with
        | Selection.Solution solution -> Selection.Solution solution
        | Selection.Project _ -> Selection.Solution state.Solution
        | Selection.Folder folder ->
            match folder.Parent with
            | Parent.Folder parent_folder -> Selection.Folder parent_folder
            | Parent.Project parent_project -> Selection.Project parent_project
        | Selection.File file ->
            match file.Parent with
            | Parent.Folder parent_folder -> Selection.Folder parent_folder
            | Parent.Project parent_project -> Selection.Project parent_project
        
    let expand_selected(state: State) : unit =
        match state.Selected with
        | Selection.Solution _ -> ()
        | Selection.Project project -> state.Expanded <- state.Expanded.Add(project.FullPath)
        | Selection.Folder folder -> state.Expanded <- state.Expanded.Add(folder.FullPath)
        | Selection.File _ -> ()
        
    let rec collapse_selected(state: State) : unit =
        match state.Selected with
        | Selection.Solution _ -> ()
        | Selection.Project project ->
            state.Expanded <- state.Expanded.Remove(project.FullPath)
            for subfolder in project.EnumerateSubfolders() do
                state.Expanded <- state.Expanded.Remove(subfolder.FullPath)
        | Selection.Folder folder ->
            if state.IsExpanded folder then
                state.Expanded <- state.Expanded.Remove(folder.FullPath)
                for subfolder in folder.EnumerateSubfolders() do
                    state.Expanded <- state.Expanded.Remove(subfolder.FullPath)
            else
                state.Selected <- navigate_out(state)
                collapse_selected(state)
        | Selection.File _ -> 
            state.Selected <- navigate_out(state)
            collapse_selected(state)

    let render(state: State) : unit =
        let rec print_fs (depth: int, entry: FileTreeEntry) =
            match entry with
            | File x ->
                if state.Selected = Selection.File x then
                    printfn " %s[%s]" (String.replicate depth "  ") x.Name
                else
                    printfn "  %s%s" (String.replicate depth "  ") x.Name
            | Folder f ->
                if state.Selected = Selection.Folder f then
                    printfn " %s[%s/]" (String.replicate depth "  ") f.Name
                else
                    printfn "  %s%s/" (String.replicate depth "  ") f.Name
                    
                if state.IsExpanded f then
                    for e in f.Children do
                        print_fs(depth + 1, e)
                    
        if state.Selected = Selection.Solution state.Solution then
            printfn "[*][%s]" state.Solution.Name
        else
            printfn "[*] %s" state.Solution.Name
            
        for project in state.Solution.Projects do
            
            if state.Selected = Selection.Project project then
                printfn " [>][%s]" project.Name
            else
                printfn " [>] %s" project.Name
                
            if state.IsExpanded project then
                for f in project.Children do
                    print_fs(0, f)
    
    let loop (solution: Solution) : unit =
        let state = State.Create(solution)
        while state.Running do
            Console.Clear()
            render state
            match state.Selected with
            | Selection.Solution s -> printfn "SLN %s" s.FullPath
            | Selection.Project s -> printfn "Project %s" s.FullPath
            | Selection.Folder s -> printfn "Folder %s" s.FullPath
            | Selection.File s -> printfn "File %s" s.FullPath
            let input = Console.ReadKey(true)
            printfn "%A+%A" input.Modifiers input.Key
            if input.Key = ConsoleKey.UpArrow then
                state.Selected <- navigate_up(state)
            if input.Key = ConsoleKey.DownArrow then
                state.Selected <- navigate_down(state)
            if input.Key = ConsoleKey.RightArrow then
                expand_selected(state)
            if input.Key = ConsoleKey.LeftArrow then
                collapse_selected(state)