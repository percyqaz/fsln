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
    
    let insert_below(project: Project, existing_file: FileTreeFile, name: string) =
        // todo: slashes in the file name should create folders
        // todo: if the file already exists, do nothing
        
        let name = name.Replace('\\', Path.AltDirectorySeparatorChar).Replace("..", "").Replace("//", "")
        let added_item_full_path = Path.Combine(Path.GetDirectoryName(existing_file.FullPath), name)
        let added_item_relative_path =
            added_item_full_path
                .Replace(Path.GetDirectoryName(project.FullPath) + Path.AltDirectorySeparatorChar.ToString(), "")
                .Replace('/', '\\')
                
        let added_item = project.ProjectRootElement.AddItem("Compile", added_item_relative_path)
        let parent = added_item.Parent
        parent.RemoveChild(added_item)
        
        parent.InsertAfterChild(added_item, existing_file.ProjectItemElement)
        let siblings = existing_file.Parent.Children
        siblings.Insert(siblings.IndexOf(File existing_file) + 1, File { Name = name; FullPath = added_item_full_path; ProjectItemElement = added_item; Parent = existing_file.Parent })
        File.Create(added_item_full_path).Dispose()
        project.ProjectRootElement.Save()
            
    let move_file_up(project: Project, file: FileTreeFile) : unit =
        let siblings = file.Parent.Children
        let folder_pos = siblings.IndexOf(File file)
        
        if folder_pos > 0 then
            siblings.RemoveAt(folder_pos)
            siblings.Insert(folder_pos - 1, File file)
            
            let parent = file.ProjectItemElement.Parent
            let previous_sibling_proj = file.ProjectItemElement.PreviousSibling
            parent.RemoveChild(file.ProjectItemElement)
            parent.InsertBeforeChild(file.ProjectItemElement, previous_sibling_proj)
            project.ProjectRootElement.Save()
            
    let move_file_down(project: Project, file: FileTreeFile) : unit =
        let siblings = file.Parent.Children
        let folder_pos = siblings.IndexOf(File file)
        
        if folder_pos + 1 < siblings.Count then
            siblings.RemoveAt(folder_pos)
            siblings.Insert(folder_pos + 1, File file)
            
            let parent = file.ProjectItemElement.Parent
            let next_sibling_proj = file.ProjectItemElement.NextSibling
            parent.RemoveChild(file.ProjectItemElement)
            parent.InsertAfterChild(file.ProjectItemElement, next_sibling_proj)
            project.ProjectRootElement.Save()
            
    let move_folder_up(project: Project, folder: FileTreeFolder) : unit =
        let siblings = folder.Parent.Children
        let files = folder.EnumerateFiles() |> Array.ofSeq
        let folder_pos = siblings.IndexOf(Folder folder)
        
        if folder_pos > 0 && files.Length > 0 then
            siblings.RemoveAt(folder_pos)
            match siblings.[folder_pos - 1] with
            | Folder merge when merge.FullPath = folder.FullPath ->
                merge.Children.AddRange(folder.Children |> Seq.map _.WithParent(Parent.Folder merge))
            | _ -> siblings.Insert(folder_pos - 1, Folder folder)
            
            let first_file = files.[0]
            let parent = first_file.ProjectItemElement.Parent
            let previous_sibling_proj = first_file.ProjectItemElement.PreviousSibling
            for file in files do
                parent.RemoveChild(file.ProjectItemElement)
                parent.InsertBeforeChild(file.ProjectItemElement, previous_sibling_proj)
            project.ProjectRootElement.Save()
            
    let move_folder_down(project: Project, folder: FileTreeFolder) : unit =
        let siblings = folder.Parent.Children
        let files = folder.EnumerateFiles() |> Array.ofSeq
        let folder_pos = siblings.IndexOf(Folder folder)
        
        if folder_pos + 1 < siblings.Count && files.Length > 0 then
            siblings.RemoveAt(folder_pos)
            match siblings.[folder_pos + 1] with
            | Folder merge when merge.FullPath = folder.FullPath ->
                merge.Children.InsertRange(0, folder.Children |> Seq.map _.WithParent(Parent.Folder merge))
            | _ -> siblings.Insert(folder_pos + 1, Folder folder)
            
            let last_file = files.[files.Length - 1]
            let parent = last_file.ProjectItemElement.Parent
            let next_sibling_proj = last_file.ProjectItemElement.NextSibling
            for file in files |> Seq.rev do
                parent.RemoveChild(file.ProjectItemElement)
                parent.InsertAfterChild(file.ProjectItemElement, next_sibling_proj)
            project.ProjectRootElement.Save()