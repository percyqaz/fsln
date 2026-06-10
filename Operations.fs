namespace fsln

open System.IO
open fsln

module Operations =
    
    let render(solution: Solution) : unit =
        let rec print_fs (depth: int, entry: FileTreeEntry) =
            match entry with
            | File x -> printfn "  %s%s" (String.replicate depth "  ") x.FullPath
            | Folder f ->
                printfn "  %s%s/" (String.replicate depth "  ") f.Name
                for e in f.Children do
                    print_fs(depth + 1, e)
                    
        printfn "[*] %s" solution.Name
        for project in solution.Projects do
            printfn " [>] %s" project.Name
            for f in project.Children do
                print_fs(0, f)
    
    let insert_after(project: Project, existing_full_file_path: string, name: string) =
        // todo: slashes in the file name should create folders
        // todo: .. in the file name should be disallowed
        // todo: if the file already exists, do nothing
        match project.FindFileAndSiblings(existing_full_file_path) with
        | Some (file, siblings) ->
            let added_item_full_path = Path.Combine(Path.GetDirectoryName(existing_full_file_path), name)
            let added_item_relative_path = added_item_full_path.Replace(Path.GetDirectoryName(project.FullPath) + Path.AltDirectorySeparatorChar.ToString(), "")
            let added_item = project.ProjectRootElement.AddItem("Compile", added_item_relative_path)
            let parent = added_item.Parent
            parent.RemoveChild(added_item)
            parent.InsertAfterChild(added_item, file.ProjectItemElement)
            siblings.Insert(siblings.IndexOf(File file) + 1, File { Name = name; FullPath = added_item_full_path; ProjectItemElement = added_item })
            File.Create(added_item_full_path).Dispose()
            project.ProjectRootElement.Save()
        | None ->
            printfn "not found!"
            
    let move_file_up(project: Project, full_file_path: string) : unit =
        match project.FindFileAndSiblings(full_file_path) with
        | Some (file, siblings) ->
            let folder_pos = siblings.IndexOf(File file)
            if folder_pos > 0 then
                siblings.RemoveAt(folder_pos)
                siblings.Insert(folder_pos - 1, File file)
                let parent = file.ProjectItemElement.Parent
                let previous_sibling_proj = file.ProjectItemElement.PreviousSibling
                parent.RemoveChild(file.ProjectItemElement)
                parent.InsertBeforeChild(file.ProjectItemElement, previous_sibling_proj)
                project.ProjectRootElement.Save()
        | None ->
            printfn "not found!"
            
    let move_file_down(project: Project, full_file_path: string) : unit =
        match project.FindFileAndSiblings(full_file_path) with
        | Some (file, siblings) ->
            let folder_pos = siblings.IndexOf(File file)
            if folder_pos + 1 < siblings.Count then
                siblings.RemoveAt(folder_pos)
                siblings.Insert(folder_pos + 1, File file)
                let parent = file.ProjectItemElement.Parent
                let next_sibling_proj = file.ProjectItemElement.NextSibling
                parent.RemoveChild(file.ProjectItemElement)
                parent.InsertAfterChild(file.ProjectItemElement, next_sibling_proj)
                project.ProjectRootElement.Save()
        | None ->
            printfn "not found!"
            
    let move_folder_up(project: Project, full_folder_path: string) : unit =
        match project.FindFolderAndSiblings(full_folder_path) with
        | Some (folder, siblings) ->
            let files = folder.EnumerateFiles() |> Array.ofSeq
            let folder_pos = siblings.IndexOf(Folder folder)
            if folder_pos > 0 && files.Length > 0 then
                siblings.RemoveAt(folder_pos)
                match siblings.[folder_pos - 1] with
                | Folder merge when merge.FullPath = folder.FullPath ->
                    merge.Children.AddRange(folder.Children)
                | _ -> siblings.Insert(folder_pos - 1, Folder folder)
                let first_file = files.[0]
                let parent = first_file.ProjectItemElement.Parent
                let previous_sibling_proj = first_file.ProjectItemElement.PreviousSibling
                for file in files do
                    parent.RemoveChild(file.ProjectItemElement)
                    parent.InsertBeforeChild(file.ProjectItemElement, previous_sibling_proj)
                project.ProjectRootElement.Save()
        | None ->
            printfn "not found!"
            
    let move_folder_down(project: Project, full_folder_path: string) : unit =
        match project.FindFolderAndSiblings(full_folder_path) with
        | Some (folder, siblings) ->
            let files = folder.EnumerateFiles() |> Array.ofSeq
            let folder_pos = siblings.IndexOf(Folder folder)
            if folder_pos + 1 < siblings.Count && files.Length > 0 then
                siblings.RemoveAt(folder_pos)
                match siblings.[folder_pos + 1] with
                | Folder merge when merge.FullPath = folder.FullPath ->
                    merge.Children.InsertRange(0, folder.Children)
                | _ -> siblings.Insert(folder_pos + 1, Folder folder)
                let last_file = files.[files.Length - 1]
                let parent = last_file.ProjectItemElement.Parent
                let next_sibling_proj = last_file.ProjectItemElement.NextSibling
                for file in files |> Seq.rev do
                    parent.RemoveChild(file.ProjectItemElement)
                    parent.InsertAfterChild(file.ProjectItemElement, next_sibling_proj)
                project.ProjectRootElement.Save()
        | None ->
            printfn "not found!"