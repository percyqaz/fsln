open System.IO
open fsln

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
    | Some solution_path ->
        let solution = SolutionTree.read_solution_file(solution_path)
        TreeOperations.render solution
        TreeOperations.insert_after(solution.Projects.[0], "/home/deck/Desktop/Source/fsln/SolutionTree.fs", "NewFile.fs")
        TreeOperations.render solution
        0