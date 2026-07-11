namespace fsln

open System
open System.Diagnostics

module Interactive =

    let key_to_buffer(state: InteractiveState) : unit =
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
            
    let dispatch_shell_command(state: InteractiveState, command: string) : unit =
        let selection_path =
            match state.Selected with
            | Selection.Solution solution -> sprintf "%A" solution.FullPath
            | Selection.Project project -> sprintf "%A" project.FullPath
            | Selection.Folder folder -> sprintf "%A" folder.FullPath
            | Selection.File file -> sprintf "%A" file.FullPath
            
        let inline apply_substitutions(command: string) : string =
            command
                .Replace("$$", '\uFFFD'.ToString())
                .Replace("$SOLUTION", state.Solution.FullPath)
                .Replace("$PROJECT", state.Solution.FullPath)
                .Replace("$", selection_path)
                .Replace('\uFFFD', '$')
            
        let shell, args = 

            if OperatingSystem.IsWindows() then 
                "cmd.exe",
                "/c " +
                apply_substitutions(command)
                    
            else 
                "/bin/sh",
                "-c \"" +
                apply_substitutions(command)
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
            
    let dispatch_internal_command(state: InteractiveState, command: string) : unit =
        match command with
        | "q" | "q!" -> state.Running <- false
        | "up" -> state.Selected <- InteractiveState.navigate_up(state)
        | "down" -> state.Selected <- InteractiveState.navigate_down(state)
        | "expand" -> InteractiveState.expand_selected(state)
        | "collapse" -> InteractiveState.collapse_selected(state)
        | "move_up" -> InteractiveState.move_selection_up(state)
        | "move_down" -> InteractiveState.move_selection_down(state)
        | "delete" -> ()
        | _ -> ()
            
    let consume_buffer(state: InteractiveState, shorthand: string, target: string) : unit =
        if state.Buffer.StartsWith(shorthand) then
            state.Buffer <- target + state.Buffer.Substring(shorthand.Length)
            
    let register_default_binds(state: InteractiveState) : unit =
        state.Bind("<Esc>", ":q<Enter>")
        state.Bind("h", ":collapse<Enter>")
        state.Bind("j", ":down<Enter>")
        state.Bind("k", ":up<Enter>")
        state.Bind("l", ":expand<Enter>")
        
        state.Bind(".", ":!echo $<Enter>")
        state.Bind("<Enter>", ":!C:/Program^ Files/JetBrains/JetBrains^ Rider^ 2026.1/bin/rider64.exe nosplash $<Enter>")

        state.Bind("<A-k>", ":move_up<Enter>")
        state.Bind("<A-j>", ":move_down<Enter>")
        
        state.Bind("<Left>", "h")
        state.Bind("<Down>", "j")
        state.Bind("<Up>", "k")
        state.Bind("<Right>", "l")
        state.Bind("<A-Up>", "<A-k>")
        state.Bind("<A-Down>", "<A-j>")
        
        state.Bind("a", "lj")
        // todo: [ ] to go next/previous sibling
        
    [<TailCall>]
    let rec dispatch_keybindings(state: InteractiveState) : unit =
        let previous_buffer = state.Buffer
        
        for bind_source, bind_target in state.Keymap do
            consume_buffer(state, bind_source, bind_target)
            
        if state.Buffer.EndsWith("<Esc>") then
            state.Buffer <- ""
            
        elif state.Buffer.StartsWith(":!") && state.Buffer.EndsWith("<Enter>") then
            let end_of_command = state.Buffer.IndexOf("<Enter>")
            let command = state.Buffer.Substring(2, end_of_command - 2)
            dispatch_shell_command(state, command)
            state.Buffer <- state.Buffer.Substring(end_of_command + 7)
            
        elif state.Buffer.StartsWith(":") && state.Buffer.Contains("<Enter>") then
            let end_of_command = state.Buffer.IndexOf("<Enter>")
            let command = state.Buffer.Substring(1, end_of_command - 1)
            dispatch_internal_command(state, command)
            state.Buffer <- state.Buffer.Substring(end_of_command + 7)
            
        if previous_buffer <> state.Buffer then
            dispatch_keybindings(state)
    
    let loop (solution: Solution) : unit =
        let state = InteractiveState.Create(solution)
        register_default_binds(state)
        let render = InteractiveDisplay(state)
        while state.Running do
            render.Redraw()
            key_to_buffer(state)
            dispatch_keybindings(state)
