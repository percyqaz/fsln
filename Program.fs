open System
open System.IO
open FSLN

let get_fsln_config () : string seq =
    let user_profile_settings =
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".fsln")

    let local_directory_settings = Path.walk_tree_specific_file(".fsln")

    seq {
        if File.Exists(user_profile_settings) then
            yield! File.ReadAllLines(user_profile_settings)

        match local_directory_settings with
        | Some file when file <> user_profile_settings && File.Exists(file) -> yield! File.ReadAllLines(file)
        | _ -> ()
    }
    |> Seq.filter(String.IsNullOrWhiteSpace >> not)

[<EntryPoint>]
let main (argv: string array) : int =
    ignore argv
    let sln = Path.walk_tree_specific_filetypes [| ".slnx"; ".sln" |]

    match sln with
    | None -> 1
    | Some solution_path ->
        let solution = SolutionLoader.read_solution_file(solution_path)
        Directory.SetCurrentDirectory(Path.GetDirectoryName(solution_path))
        Interactive.loop(get_fsln_config(), solution)
        0

// todo list:
// file search showing a filtered view
// -- GIT MODE --, filter tree to just git changed files, keys to stage files, quick-commit, hotkey to show git log
// -- ERROR MODE --, run programs and parse their MSBuild-style output, ability to browse this list and hit enter to open editor on these files
// -- FILE MODE --, general purpose file tree browser ? or too big an endeavour
