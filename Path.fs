namespace FSLN

open System
open System.IO

module Path =

    let normalise (path: string) : string =
        Uri(Path.GetFullPath(path)).LocalPath.Replace('\\', Path.AltDirectorySeparatorChar)

    let get_directory_name (path: string) : string =
        Path.GetDirectoryName(path).Replace('\\', Path.AltDirectorySeparatorChar)

    let find_git_repo () : string option =
        let mutable current_path = Path.GetFullPath(".")

        while current_path <> null && not(Directory.Exists(Path.Combine(current_path, ".git"))) do
            current_path <- Path.GetDirectoryName(current_path)

        if current_path = null then None else Some(current_path)

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
