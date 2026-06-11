namespace fsln

open System

module Interactive =
    
    type State =
        {
            mutable Running: bool
            Solution: Solution
            mutable Expanded: Set<string>
            mutable Selected: string
        }
        
        static member Create(solution: Solution) = { Running = true; Solution = solution; Expanded = Set.empty; Selected = solution.FullPath }
        
    let render(state: State) : unit =
        let rec print_fs (depth: int, entry: FileTreeEntry) =
            match entry with
            | File x ->
                if state.Selected = x.FullPath then
                    printfn " %s[%s]" (String.replicate depth "  ") x.Name
                else
                    printfn "  %s%s" (String.replicate depth "  ") x.Name
            | Folder f ->
                if state.Selected = f.FullPath then
                    printfn " %s[%s/]" (String.replicate depth "  ") f.Name
                else
                    printfn "  %s%s/" (String.replicate depth "  ") f.Name
                    
                if state.Expanded.Contains f.FullPath then
                    for e in f.Children do
                        print_fs(depth + 1, e)
                    
        if state.Selected = state.Solution.FullPath then
            printfn "[*][%s]" state.Solution.Name
        else
            printfn "[*] %s" state.Solution.Name
            
        for project in state.Solution.Projects do
            
            if state.Selected = project.FullPath then
                printfn " [>][%s]" project.Name
            else
                printfn " [>] %s" project.Name
                
            if state.Expanded.Contains project.FullPath then
                for f in project.Children do
                    print_fs(0, f)
    
    let loop (solution: Solution) =
        let state = State.Create(solution)
        while state.Running do
            render state
            let input = Console.ReadKey(true)
            printfn "%A+%A" input.Modifiers input.Key