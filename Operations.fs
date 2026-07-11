namespace fsln

open System.IO
open Microsoft.Build.Construction
open fsln

module Operations =
    
    let insert_below(project: Project, existing_file: FileTreeFile, name: string) : unit =
        // todo: slashes in the file name should create folders
        // todo: if the file already exists, do nothing
        let name = name.Replace('\\', Path.AltDirectorySeparatorChar).Replace("..", "").Replace("//", "")
        // todo: review this path.combine on windows vs unix
        let added_item_full_path = Path.Combine(Path.get_directory_name(existing_file.FullPath), name)
        let added_item_relative_path =
            added_item_full_path
                .Replace(Path.get_directory_name(project.FullPath) + Path.AltDirectorySeparatorChar.ToString(), "")
                .Replace('/', '\\')
                
        let added_item = project.ProjectRootElement.AddItem("Compile", added_item_relative_path)
        let parent = added_item.Parent
        parent.RemoveChild(added_item)
        
        parent.InsertAfterChild(added_item, existing_file.ProjectItemElement)
        let siblings = existing_file.Parent.Children
        siblings.Insert(siblings.IndexOf(File existing_file) + 1, File { Name = name; FullPath = added_item_full_path; ProjectItemElement = added_item; Parent = existing_file.Parent })
        File.Create(added_item_full_path).Dispose()
        project.ProjectRootElement.Save()
        
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