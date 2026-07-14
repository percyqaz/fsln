namespace FSLN

open System

module Interactive =

    let loop (config: string seq, solution: Solution) : unit =
        let state = InteractiveState.Create(solution)
        Commands.register_default_binds(state)

        state.Buffer <- String.concat InputBuffer.ENTER config + InputBuffer.ENTER
        InputBuffer.dispatch_keybindings(state)

        let render = InteractiveDisplay(state)
        let input_thread = InputThread()
        input_thread.Start()

        Console.Write("\u001b[?1049h")

        while state.Running do
            render.Redraw()

            match input_thread.TryReadKey(2000) with
            | true, input ->
                InputBuffer.add_input_to_buffer(input, state)
                InputBuffer.dispatch_keybindings(state)
            | false, _ ->
                state.GitStatus <- GitStatus.Fetch()

                if state.Solution.Projects |> Seq.exists _.HasExternalChange() then
                    state.Solution <- SolutionLoader.read_solution_file(state.Solution.FullPath)
                    state.Selected <- Selection.Solution(state.Solution)

        Console.Write("\u001b[?1049l")

        input_thread.Dispose()
