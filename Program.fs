open System.IO

let walk_tree_specific_file(target: string) : string option =
    let mutable current_path = Path.GetFullPath(".")
    while current_path <> null && not (File.Exists(Path.Combine(current_path, target))) do
        current_path <- Path.GetDirectoryName(current_path)
    if current_path = null then
        None
    else
        Some(Path.Combine(current_path, target))

let walk_tree_specific_filetypes(targets: string array) : string option =
    let mutable current_path = Path.GetFullPath(".")
    let mutable result: string option = None
    while current_path <> null && result.IsNone do
        for file in Directory.EnumerateFiles(current_path) do
            let ext = Path.GetExtension(file).ToLower()
            if Array.contains ext targets then
                result <- Some(Path.Combine(current_path, file))
        current_path <- Path.GetDirectoryName(current_path)
    result

[<EntryPoint>]  
let main (_: string array) : int =
    let sln =
        walk_tree_specific_filetypes [|".slnx"; ".sln"|]
    match sln with
    | None -> 1
    | Some solution ->
        
        let rec print_fs (depth: int, entry: fsln.FileTreeEntry) =
            match entry with
            | fsln.File x -> printfn "  %s%s" (String.replicate depth "  ") x.Name
            | fsln.Folder f ->
                printfn "  %s%s/" (String.replicate depth "  ") f.Name
                for e in f.Children do
                    print_fs(depth + 1, e)
                    
        let sln = fsln.SolutionTree.read_solution_file(solution)
        printfn "[*] %s" sln.Name
        for project in sln.Projects do
            printfn " [>] %s" project.Name
            for f in project.Children do
                print_fs(0, f)
        0