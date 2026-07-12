namespace fsln

open System

module InputBuffer =

    [<Literal>]
    let LT_LOOKALIKE = "＜"

    [<Literal>]
    let GT_LOOKALIKE = "＞"

    let inline private special (s: string) = LT_LOOKALIKE + s + GT_LOOKALIKE

    let ENTER = special("Enter")

    let key_to_buffer (state: InteractiveState) : unit =
        let input = Console.ReadKey(true)

        if input.Key = ConsoleKey.Backspace then
            if state.Buffer.EndsWith(GT_LOOKALIKE) then
                let p = state.Buffer.LastIndexOf(LT_LOOKALIKE)

                if p >= 0 then
                    state.Buffer <- state.Buffer.Substring(0, p)
                else
                    state.Buffer <- state.Buffer.Substring(0, state.Buffer.Length - 1)
            elif state.Buffer <> "" then
                state.Buffer <- state.Buffer.Substring(0, state.Buffer.Length - 1)

        elif input.Modifiers &&& ConsoleModifiers.Control = ConsoleModifiers.Control then
            ()

        elif input.Key = ConsoleKey.Escape then
            state.Buffer <- state.Buffer + special("Esc")
        elif input.Key = ConsoleKey.Spacebar then
            state.Buffer <- state.Buffer + " "
        elif input.KeyChar <> '\u0000' && Char.IsAscii(input.KeyChar) && not(Char.IsWhiteSpace(input.KeyChar)) then
            if input.Modifiers &&& ConsoleModifiers.Alt = ConsoleModifiers.Alt then
                state.Buffer <- state.Buffer + special(sprintf "A-%c" input.KeyChar)
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

            let alt =
                if input.Modifiers &&& ConsoleModifiers.Alt = ConsoleModifiers.Alt then "A-" else ""

            state.Buffer <- state.Buffer + special(sprintf "%s%s" alt key)

    let consume_buffer (state: InteractiveState, shorthand: string, target: string) : unit =
        if state.Buffer.StartsWith(shorthand) then
            state.Buffer <- target + state.Buffer.Substring(shorthand.Length)

    [<TailCall>]
    let rec dispatch_keybindings (state: InteractiveState) : unit =
        let previous_buffer = state.Buffer

        for bind_source, bind_target in state.Keymap do
            consume_buffer(state, bind_source, bind_target)

        if state.Buffer.EndsWith(special("Esc")) then
            state.Buffer <- ""

        elif state.Buffer.StartsWith(":!") && state.Buffer.EndsWith(ENTER) then
            let end_of_command = state.Buffer.IndexOf(ENTER)
            let command = state.Buffer.Substring(2, end_of_command - 2)
            Commands.dispatch_shell_command(state, command)
            state.Buffer <- state.Buffer.Substring(end_of_command + ENTER.Length)

        elif state.Buffer.StartsWith(":") && state.Buffer.Contains(ENTER) then
            let end_of_command = state.Buffer.IndexOf(ENTER)
            let command = state.Buffer.Substring(1, end_of_command - 1)
            Commands.dispatch_internal_command(state, command)
            state.Buffer <- state.Buffer.Substring(end_of_command + ENTER.Length)

        if previous_buffer <> state.Buffer then
            if state.Buffer.Length > 4000 then
                printfn "%s" state.Buffer
                state.Running <- false
            else
                dispatch_keybindings(state)
