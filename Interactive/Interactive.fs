namespace fsln

module Interactive =
    
    let loop (solution: Solution) : unit =
        let state = InteractiveState.Create(solution)
        Commands.register_default_binds(state)
        let render = InteractiveDisplay(state)
        while state.Running do
            render.Redraw()
            InputBuffer.key_to_buffer(state)
            InputBuffer.dispatch_keybindings(state)