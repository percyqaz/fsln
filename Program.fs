open System
open System.IO
open FSLN

let walk_tree_specific_file (target: string) : string option =
    let mutable current_path = Path.GetFullPath(".")

    while current_path <> null && not(File.Exists(Path.Combine(current_path, target))) do
        current_path <- Path.GetDirectoryName(current_path)

    if current_path = null then None else Some(Path.Combine(current_path, target))

let walk_tree_specific_filetypes (targets: string array) : string option =
    let mutable current_path = Path.GetFullPath(".")
    let mutable result: string option = None

    while current_path <> null && result.IsNone do
        for file in Directory.EnumerateFiles(current_path) do
            let ext = Path.GetExtension(file).ToLower()

            if Array.contains ext targets then
                result <- Some(Path.Combine(current_path, file))

        current_path <- Path.GetDirectoryName(current_path)

    result

let get_fsln_config() : string seq =    
    let user_profile_settings = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".fsln")
    let local_directory_settings = walk_tree_specific_file(".fsln")
    seq {
        if File.Exists(user_profile_settings) then
            yield! File.ReadAllLines(user_profile_settings)
            
        match local_directory_settings with
        | Some file when file <> user_profile_settings && File.Exists(file) ->
            yield! File.ReadAllLines(file)
        | _ -> ()
    }
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)

[<EntryPoint>]
let main (argv: string array) : int =
    ignore argv
    let sln = walk_tree_specific_filetypes [| ".slnx"; ".sln" |]

    match sln with
    | None -> 1
    | Some solution_path ->
        let solution = SolutionLoader.read_solution_file(solution_path)
        Directory.SetCurrentDirectory(Path.GetDirectoryName(solution_path))
        Interactive.loop(get_fsln_config(), solution)
        0

// todo list:
// file search showing a filtered view
// git information, run git status and annotate the tree with it
// status line, showing git branch + is dirty + commits ahead of remote
// auto-reload file tree if external modification detected, check every 2s
// -- GIT MODE --, filter tree to just git changed files, keys to stage files, quick-commit, hotkey to show git log
// -- ERROR MODE --, run programs and parse their MSBuild-style output, ability to browse this list and hit enter to open editor on these files
// -- FILE MODE --, general purpose file tree browser ? or too big an endeavour
