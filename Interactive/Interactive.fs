namespace FSLN

module Interactive =

    let loop (config: string seq, solution: Solution) : unit =
        let state = InteractiveState.Create(solution)
        Commands.register_default_binds(state)

        state.Buffer <- String.concat InputBuffer.ENTER config + InputBuffer.ENTER
        InputBuffer.dispatch_keybindings(state)

        let render = InteractiveDisplay(state)
        let input_thread = InputThread()
        input_thread.Start()

        System.Console.Write("\u001b[?1049h")

        while state.Running do
            render.Redraw()

            match input_thread.TryReadKey(5000) with
            | true, input ->
                InputBuffer.add_input_to_buffer(input, state)
                InputBuffer.dispatch_keybindings(state)
            | false, _ -> () // do background work every 5 seconds of idle

        System.Console.Write("\u001b[?1049l")

        input_thread.Dispose()
