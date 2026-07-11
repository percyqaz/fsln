namespace fsln

open System
open System.IO
open Microsoft.Build.Construction
open fsln

module Operations =
    
    let inline private validate_name(name: string) : bool =
        name.Trim().TrimEnd('.').Replace("..", "") = name
        && String.forall (fun c -> Char.IsAsciiLetterOrDigit(c) || c = '.' || c = '_' || c = ' ') name
    
    let rec private resolve_path(parent: Parent, parts: string list) : Result<Parent * string, string> =
        match parts with
        | [name] ->
            if validate_name(name) then Ok(parent, name)
            else Error "Invalid file name"
        | path_segment :: remaining ->
            if path_segment = ".." then
                match parent with
                | Parent.Folder folder -> resolve_path(folder.Parent, remaining)
                | Parent.Project _ -> Error "Path is outside project!"
                
            elif validate_name(path_segment) then
                let existing_folder =
                    parent.Children
                    |> Seq.tryPick (function FileTreeEntry.Folder folder when folder.Name = path_segment -> Some folder | _ -> None)
                match existing_folder with
                | Some folder -> resolve_path(Parent.Folder folder, remaining)
                | None ->
                    let new_path =
                        match parent with
                        | Parent.Folder folder -> folder.FullPath
                        | Parent.Project project -> Path.get_directory_name(project.FullPath)
                        + "/" + path_segment

                    let new_folder : FileTreeFolder = { Name = path_segment; Parent = parent; FullPath = new_path; Children = ResizeArray() }
                    resolve_path(Parent.Folder new_folder, remaining)
            else
                Error "Invalid path segment"
                
        | [] -> Error "empty parts passed!"
                
    let add_file (project: Project, parent: Parent, path: string) : Result<unit, string> =
        let directory_parts =
            path.Replace('\\', Path.AltDirectorySeparatorChar).Split(Path.AltDirectorySeparatorChar, StringSplitOptions.None)
            |> List.ofArray
        match resolve_path(parent, directory_parts) with
        | Error reason -> Error reason
        | Ok(new_parent, file_name) ->
            let new_parent_full_path =
                match new_parent with
                | Parent.Folder folder -> folder.FullPath
                | Parent.Project project -> Path.get_directory_name(project.FullPath)

            let added_item_full_path = Path.Combine(new_parent_full_path, file_name)
            if new_parent.Children |> Seq.exists(function FileTreeEntry.File file when file.Name = file_name -> true | _ -> false) || File.Exists(added_item_full_path) then
                Error "File already exists"
            else
                
            let added_item_relative_path =
                added_item_full_path
                    .Replace(Path.get_directory_name(project.FullPath) + Path.AltDirectorySeparatorChar.ToString(), "")
                    .Replace('/', '\\')
                    
            let rec find_lowest_neighbor(p: Parent) : ProjectItemElement =
                let children = p.Children
                if children.Count = 0 then
                    match p with
                    | Parent.Project _ -> failwith "adding to a project with no files, should be impossible?"
                    | Parent.Folder folder -> find_lowest_neighbor(folder.Parent)
                else
                    let last_child = children.[children.Count - 1]
                    match last_child with
                    | FileTreeEntry.File file -> file.ProjectItemElement
                    | FileTreeEntry.Folder folder -> find_lowest_neighbor(Parent.Folder folder)
                    
            let inline insert_after_neighbor(neighbor: ProjectItemElement) : ProjectItemElement =
                let added_item = project.ProjectRootElement.AddItem("Compile", added_item_relative_path)
                let parent = added_item.Parent
                parent.RemoveChild(added_item)
                parent.InsertAfterChild(added_item, neighbor)
                added_item
                
            let rec connect_to_tree(parent: Parent, item: FileTreeEntry) =
                let children = parent.Children
                let parent_needs_connecting = children.Count = 0
                children.Add(item)
                if parent_needs_connecting then
                    match parent with
                    | Parent.Project _ -> failwith "adding to a project with no files, should be impossible?"
                    | Parent.Folder folder -> connect_to_tree(folder.Parent, Folder folder)
                
            let added_project_item = insert_after_neighbor(find_lowest_neighbor(new_parent))
            let tree_file = File { Name = file_name; FullPath = added_item_full_path; ProjectItemElement = added_project_item; Parent = new_parent }
            File.Create(added_item_full_path).Dispose()
            project.ProjectRootElement.Save()
            connect_to_tree(new_parent, tree_file)
            Ok()
        
    let inline private swap_files_in_project(above_files: ProjectItemElement seq, below_files: ProjectItemElement seq) : unit =
        let first_above_file = Seq.head above_files
        let parent = first_above_file.Parent
        for file in below_files do
            parent.RemoveChild(file)
            parent.InsertBeforeChild(file, first_above_file)
        
    let inline private merge_folders_if_needed(entry_one: FileTreeEntry, entry_two: FileTreeEntry, siblings: ResizeArray<FileTreeEntry>) : unit =
        match entry_one, entry_two with
        | Folder a, Folder b when a.FullPath = b.FullPath ->
            a.Children.AddRange(b.Children |> Seq.map _.WithParent(Parent.Folder a))
            siblings.Remove(entry_two) |> ignore
        | _ -> ()
            
    let move_file_up(project: Project, file: FileTreeFile) : unit =
        let siblings = file.Parent.Children
        let folder_pos = siblings.IndexOf(File file)
        
        if folder_pos > 0 then
            siblings.RemoveAt(folder_pos)
            siblings.Insert(folder_pos - 1, File file)
            
            let swapped_with = siblings.[folder_pos]
            match swapped_with with
            | Folder other_folder ->
                swap_files_in_project(
                    other_folder.EnumerateFiles() |> Seq.map _.ProjectItemElement,
                    [file.ProjectItemElement]
                )
            | File other_file ->
                swap_files_in_project(
                    [other_file.ProjectItemElement],
                    [file.ProjectItemElement]
                )
                
            if folder_pos + 1 < siblings.Count then
                merge_folders_if_needed(siblings.[folder_pos], siblings.[folder_pos + 1], siblings)
                
            project.ProjectRootElement.Save()
            
    let move_file_down(project: Project, file: FileTreeFile) : unit =
        let siblings = file.Parent.Children
        let folder_pos = siblings.IndexOf(File file)
        
        if folder_pos + 1 < siblings.Count then
            siblings.RemoveAt(folder_pos)
            siblings.Insert(folder_pos + 1, File file)
            
            let swapped_with = siblings.[folder_pos]
            match swapped_with with
            | Folder other_folder ->
                swap_files_in_project(
                    [file.ProjectItemElement],
                    other_folder.EnumerateFiles() |> Seq.map _.ProjectItemElement
                )
            | File other_file ->
                swap_files_in_project(
                    [file.ProjectItemElement],
                    [other_file.ProjectItemElement]
                )
                
            if folder_pos >= 1 then
                merge_folders_if_needed(siblings.[folder_pos - 1], siblings.[folder_pos], siblings)
                
            project.ProjectRootElement.Save()
            
    let move_folder_up(project: Project, folder: FileTreeFolder) : unit =
        let siblings = folder.Parent.Children
        let folder_pos = siblings.IndexOf(Folder folder)
        
        if folder_pos > 0 then
            siblings.RemoveAt(folder_pos)
            siblings.Insert(folder_pos - 1, Folder folder)
            
            let swapped_with = siblings.[folder_pos]
            match swapped_with with
            | Folder other_folder ->
                swap_files_in_project(
                    other_folder.EnumerateFiles() |> Seq.map _.ProjectItemElement,
                    folder.EnumerateFiles() |> Seq.map _.ProjectItemElement
                )
            | File other_file ->
                swap_files_in_project(
                    [other_file.ProjectItemElement],
                    folder.EnumerateFiles() |> Seq.map _.ProjectItemElement
                )
            
            if folder_pos >= 2 then
                merge_folders_if_needed(siblings.[folder_pos - 1], siblings.[folder_pos - 2], siblings)
            if folder_pos + 1 < siblings.Count then
                merge_folders_if_needed(siblings.[folder_pos], siblings.[folder_pos + 1], siblings)
                
            project.ProjectRootElement.Save()
            
    let move_folder_down(project: Project, folder: FileTreeFolder) : unit =
        let siblings = folder.Parent.Children
        let folder_pos = siblings.IndexOf(Folder folder)
        
        if folder_pos + 1 < siblings.Count then
            siblings.RemoveAt(folder_pos)
            siblings.Insert(folder_pos + 1, Folder folder)
            
            let swapped_with = siblings.[folder_pos]
            match swapped_with with
            | Folder other_folder ->
                swap_files_in_project(
                    folder.EnumerateFiles() |> Seq.map _.ProjectItemElement,
                    other_folder.EnumerateFiles() |> Seq.map _.ProjectItemElement
                )
            | File other_file ->
                swap_files_in_project(
                    folder.EnumerateFiles() |> Seq.map _.ProjectItemElement,
                    [other_file.ProjectItemElement]
                )
            
            if folder_pos + 2 < siblings.Count then
                merge_folders_if_needed(siblings.[folder_pos + 1], siblings.[folder_pos + 2], siblings)
            if folder_pos >= 1 then
                merge_folders_if_needed(siblings.[folder_pos - 1], siblings.[folder_pos], siblings)
                
            project.ProjectRootElement.Save()