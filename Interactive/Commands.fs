namespace FSLN

open System
open System.Diagnostics

module Commands =

    let inline apply_substitutions (state: InteractiveState, command: string) : string =
        command
            .Replace("$$", '\uFFFD'.ToString())
            .Replace("$SOLUTION", state.Solution.FullPath)
            .Replace(
                "$PROJECT",
                match state.Selected.ParentProject with
                | Some project -> project.FullPath
                | None -> ""
            )
            .Replace("$", state.Selected.FullPath)
            .Replace('\uFFFD', '$')

    let dispatch_shell_command (state: InteractiveState, command: string) : unit =

        let shell, args =

            if OperatingSystem.IsWindows() then
                "cmd.exe", "/c " + apply_substitutions(state, command)

            else
                "/bin/sh", "-c \"" + apply_substitutions(state, command) + "\""

        let start_info = ProcessStartInfo(shell, args)
        Console.Write("\u001b[?1049l\u001b[47h\u001b[2J\u001b[H")
        let proc = Process.Start(start_info)
        proc.WaitForExit()

        if proc.ExitCode <> 0 then
            Console.ReadKey(true) |> ignore
            state.StatusLine <- sprintf "(%i)" proc.ExitCode
        elif Console.GetCursorPosition() <> struct (0, 0) then
            Console.WriteLine("Press any key to return".ForeColor(0x666666))
            Console.ReadKey(true) |> ignore

        Console.Write("\u001b[47l\u001b[?1049h")

    let dispatch_internal_command (state: InteractiveState, command: string) : unit =
        let split = command.Split(" ", 2, StringSplitOptions.TrimEntries)
        let args = apply_substitutions(state, if split.Length < 2 then "" else split.[1])

        match split.[0] with
        | "q"
        | "q!" -> state.Running <- false
        | "up" -> state.Selected <- InteractiveState.navigate_up(state)
        | "down" -> state.Selected <- InteractiveState.navigate_down(state)
        | "expand" -> InteractiveState.expand_selected(state)
        | "collapse" -> InteractiveState.collapse_selected(state)
        | "move_up" -> InteractiveState.move_selection_up(state)
        | "move_down" -> InteractiveState.move_selection_down(state)
        | "echo" -> state.StatusLine <- args
        | "delete" -> ()
        | "add" ->
            match state.Selected.ParentProject, state.Selected.ToParent() with
            | Some project, Some parent ->
                match Operations.add_file(project, parent, args) with
                | Ok() -> state.StatusLine <- "Created file!"
                | Error reason -> state.StatusLine <- reason
            | _ -> ()
        | "move" ->
            match state.Selected with
            | Selection.File file ->
                match Operations.move_file(file.ParentProject, file, args) with
                | Ok() -> state.StatusLine <- "Moved file!"
                | Error reason -> state.StatusLine <- reason
            | _ -> ()
        | "set" ->
            let split = args.Split("=", 2, StringSplitOptions.TrimEntries)
            let key, value = split.[0], if split.Length > 1 then split.[1] else ""

            match state.Theme.Set(key, value) with
            | Ok new_theme ->
                state.Theme <- new_theme
                state.StatusLine <- ""
            | Error reason -> state.StatusLine <- reason
        | "bind" ->
            let split = args.Split("=", 2, StringSplitOptions.TrimEntries)
            let source, target = split.[0], if split.Length > 1 then split.[1] else ""

            if source.Length > 0 && target.Length > 0 && source <> target then
                state.Bind(source, target)
                state.StatusLine <- "Binding set."
            else
                state.StatusLine <- "Invalid binding."

        | _ -> ()

    let register_default_binds (state: InteractiveState) : unit =
        state.Bind("<Esc>", ":q<Enter>")
        state.Bind("h", ":collapse<Enter>")
        state.Bind("j", ":down<Enter>")
        state.Bind("k", ":up<Enter>")
        state.Bind("l", ":expand<Enter>")

        state.Bind(".", ":!echo $<Enter>")

        state.Bind(
            "<Enter>",
            ":!C:/Program^ Files/JetBrains/JetBrains^ Rider^ 2026.1/bin/rider64.exe nosplash $<Enter>"
        )

        state.Bind("<A-k>", ":move_up<Enter>")
        state.Bind("<A-j>", ":move_down<Enter>")

        state.Bind("<Left>", "h")
        state.Bind("<Down>", "j")
        state.Bind("<Up>", "k")
        state.Bind("<Right>", "l")
        state.Bind("<A-Up>", "<A-k>")
        state.Bind("<A-Down>", "<A-j>")

        state.Bind("a", "lj")
// todo: [ ] to jump next/previous sibling
