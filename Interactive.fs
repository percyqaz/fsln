namespace fsln

open System
open System.Diagnostics
open fsln.Operations

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
            mutable Buffer: string
            mutable StatusLine: string
            Keymap: ResizeArray<string * string>
        }
        member this.IsExpanded(folder: FileTreeFolder) : bool = this.Expanded.Contains folder.FullPath
        member this.IsExpanded(project: Project) : bool = this.Expanded.Contains project.FullPath
        static member Create(solution: Solution) : State =
            {
                Running = true
                Solution = solution
                Expanded = Set.empty
                Selected = Selection.Solution solution
                Buffer = ""
                StatusLine = ""
                Keymap = ResizeArray()
            }
        
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
                    
    let move_selection_up(state: State) : unit =
        match state.Selected with
        | Selection.Solution _ -> ()
        | Selection.Project _ -> () // todo: reorder projects?
        | Selection.Folder folder -> state.Selected <- Selection.Folder(move_folder_up(folder.ParentProject, folder))
        | Selection.File file -> move_file_up(file.ParentProject, file)
        
    let move_selection_down(state: State) : unit =
        match state.Selected with
        | Selection.Solution _ -> ()
        | Selection.Project _ -> () // todo: reorder projects?
        | Selection.Folder folder -> state.Selected <- Selection.Folder(move_folder_down(folder.ParentProject, folder))
        | Selection.File file -> move_file_down(file.ParentProject, file)

    let key_to_buffer(state: State) : unit =
        let input = Console.ReadKey(true)
        
        if input.Key = ConsoleKey.Backspace then
            if state.Buffer.EndsWith(">") then
                let p = state.Buffer.LastIndexOf("<")
                if p >= 0 then
                    state.Buffer <- state.Buffer.Substring(0, p)
                else
                    state.Buffer <- state.Buffer.Substring(0, state.Buffer.Length - 1)
            elif state.Buffer <> "" then
                state.Buffer <- state.Buffer.Substring(0, state.Buffer.Length - 1)
        
        elif input.Modifiers &&& ConsoleModifiers.Control = ConsoleModifiers.Control then
            ()
                
        elif input.Key = ConsoleKey.Escape then
            state.Buffer <- state.Buffer + "<Esc>"
        elif input.Key = ConsoleKey.Spacebar then
            state.Buffer <- state.Buffer + " "
        elif input.KeyChar <> '\u0000' && Char.IsAscii(input.KeyChar) && not (Char.IsWhiteSpace(input.KeyChar)) then
            if input.Modifiers &&& ConsoleModifiers.Alt = ConsoleModifiers.Alt then
                state.Buffer <- state.Buffer + sprintf "<A-%c>" input.KeyChar
            else
                state.Buffer <- state.Buffer + input.KeyChar.ToString()
        elif input.Key <> ConsoleKey.None then
            let key =
                match input.Key with
                | ConsoleKey.LeftArrow -> "Left"
                | ConsoleKey.RightArrow -> "Right"
                | ConsoleKey.UpArrow -> "Up"
                | ConsoleKey.DownArrow -> "Down"
                | otherwise -> otherwise.ToString()
            let alt = if input.Modifiers &&& ConsoleModifiers.Alt = ConsoleModifiers.Alt then "A-" else ""
            state.Buffer <- state.Buffer + sprintf "<%s%s>" alt key
            
    let dispatch_shell_command(state: State, command: string) : unit =
        let selection_path =
            match state.Selected with
            | Selection.Solution solution -> sprintf "%A" solution.FullPath
            | Selection.Project project -> sprintf "%A" project.FullPath
            | Selection.Folder folder -> sprintf "%A" folder.FullPath
            | Selection.File file -> sprintf "%A" file.FullPath
            
        let shell, first_arg = if OperatingSystem.IsWindows() then "cmd.exe", "-c" else "/bin/sh", "-c"

        let args =
            first_arg + " \"" +
            command
                .Replace("$$", '\uFFFD'.ToString())
                .Replace("$", selection_path)
                .Replace('\uFFFD', '$')
                .Replace("\"", "\\\"")
            + "\""
        let start_info = ProcessStartInfo(shell, args)
        start_info.UseShellExecute <- false
        start_info.CreateNoWindow <- false
        
        Console.Clear()
        let proc = Process.Start(start_info)
        proc.WaitForExit()
        if proc.ExitCode <> 0 then
            Console.ReadKey(true) |> ignore
            state.StatusLine <- sprintf "(%i)" proc.ExitCode
        elif Console.GetCursorPosition() <> struct(0, 0) then
            Console.ReadKey(true) |> ignore
            
    let dispatch_internal_command(state: State, command: string) : unit =
        match command with
        | "q" | "q!" -> state.Running <- false
        | "up" -> state.Selected <- navigate_up(state)
        | "down" -> state.Selected <- navigate_down(state)
        | "expand" -> expand_selected(state)
        | "collapse" -> collapse_selected(state)
        | "move_up" -> move_selection_up(state)
        | "move_down" -> move_selection_down(state)
        | "delete" -> ()
        | _ -> ()
            
    let consume_buffer(state: State, shorthand: string, target: string) : unit =
        if state.Buffer.StartsWith shorthand then
            state.Buffer <- target + state.Buffer.Substring(shorthand.Length)
        
    let dispatch_keybindings(state: State) : unit =
        
        consume_buffer(state, "<Left>", "h")
        consume_buffer(state, "<Down>", "j")
        consume_buffer(state, "<Up>", "k")
        consume_buffer(state, "<Right>", "l")
        consume_buffer(state, "<A-Down>", "<A-j>")
        consume_buffer(state, "<A-Up>", "<A-k>")
        
        consume_buffer(state, "h", ":collapse<Enter>")
        consume_buffer(state, "j", ":down<Enter>")
        consume_buffer(state, "k", ":up<Enter>")
        consume_buffer(state, "l", ":expand<Enter>")
        consume_buffer(state, "<A-j>", ":move_down<Enter>")
        consume_buffer(state, "<A-k>", ":move_up<Enter>")
        consume_buffer(state, "<Enter>", ":!vim $<Enter>")
            
    let handle_input(state: State) : unit =
        key_to_buffer(state)
        if state.Buffer.EndsWith "<Esc>" then
            if state.Buffer = "<Esc>" then
                state.Running <- false
            else
                state.Buffer <- ""
                
        dispatch_keybindings(state)
        
        if state.Buffer.StartsWith ":!" && state.Buffer.EndsWith "<Enter>" then
            let command = state.Buffer.Substring(2, state.Buffer.Length - 9)
            dispatch_shell_command(state, command)
            state.Buffer <- ""
        elif state.Buffer.StartsWith ":" && state.Buffer.EndsWith "<Enter>" then
            let command = state.Buffer.Substring(1, state.Buffer.Length - 8)
            dispatch_internal_command(state, command)
            state.Buffer <- ""
    
    let loop (solution: Solution) : unit =
        let state = State.Create(solution)
        while state.Running do
            Console.Clear()
            render state
            if state.Buffer <> "" then
                printfn "%s" state.Buffer
            else
                printfn "%s" state.StatusLine
            handle_input(state)