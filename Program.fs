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
let main (argv: string array) : int =
    ignore argv
    let sln =
        walk_tree_specific_filetypes [|".slnx"; ".sln"|]
    match sln with
    | None -> 1
    | Some solution_path ->
        let solution = SolutionLoader.read_solution_file(solution_path)
        Directory.SetCurrentDirectory(Path.GetDirectoryName(solution_path))
        Interactive.loop solution
        0