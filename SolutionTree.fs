namespace fsln

open System
open System.IO
open Microsoft.Build.Construction

// https://git-scm.com/docs/git-status
    
type FileTreeFile =
    {
        Name: string
        FullPath: string
        ProjectItemElement: ProjectItemElement
    }
    
type FileTreeFolder =
    {
        Name: string
        FullPath: string
        Children: ResizeArray<FileTreeEntry>
    }
    
and FileTreeEntry =
    | File of FileTreeFile
    | Folder of FileTreeFolder
    
type Project =
    {
        Name: string
        FullPath: string
        ProjectRootElement: ProjectRootElement
        Children: ResizeArray<FileTreeEntry>
    }
    member this.FindFileAndSiblings(full_file_path: string) : (FileTreeFile * ResizeArray<FileTreeEntry>) option =
        let rec search (search_space: ResizeArray<FileTreeEntry>) =
            let mutable found = None
            for entry in search_space do
                match entry with
                | File f when f.FullPath = full_file_path -> found <- Some (f, search_space)
                | File _ -> ()
                | Folder f -> if found.IsNone then found <- search f.Children
            found
        search this.Children
    
type Solution =
    {
        Name: string
        FullPath: string
        SolutionFile: SolutionFile
        Projects: ResizeArray<Project>
    }
    
module SolutionTree =
    
    let normalise_path(path: string) : string =
        Uri(Path.GetFullPath(path)).LocalPath.Replace('\\', Path.AltDirectorySeparatorChar)
    
    let read_project_file(name: string, project_path: string) : Project =
        let project_path = normalise_path(project_path)
        let project_containing_folder = Path.GetDirectoryName(project_path)
        let project_file = ProjectRootElement.Open(project_path)
        
        let file_tree = ResizeArray<FileTreeEntry>()
        
        let is_subdirectory(parent_path: string, child_path: string) =
            let parent_path = if Path.EndsInDirectorySeparator(parent_path) then parent_path else parent_path + Path.AltDirectorySeparatorChar.ToString()
            let child_path = if Path.EndsInDirectorySeparator(child_path) then child_path else child_path + Path.AltDirectorySeparatorChar.ToString()
            child_path <> parent_path && child_path.StartsWith(parent_path)
            
        let nested_file(relative_to: string, file_path: string, element: ProjectItemElement) : FileTreeEntry =
            let mutable addition = File { Name = Path.GetFileName(file_path); FullPath = file_path; ProjectItemElement = element }
            let mutable containing_folder = Path.GetDirectoryName(file_path)
            while is_subdirectory(relative_to, containing_folder) do
                addition <- Folder { Name = Path.GetFileName(containing_folder); FullPath = containing_folder; Children = ResizeArray([addition]) }
                containing_folder <- Path.GetDirectoryName(containing_folder)
            addition
            
        let rec merge_trees(target: ResizeArray<FileTreeEntry>, item: FileTreeEntry) =
            match item with
            | File _ -> target.Add(item)
            | Folder _ when target.Count = 0 -> target.Add(item)
            | Folder f ->
                let last = target.[target.Count - 1]
                match last with
                | File _ -> target.Add(item)
                | Folder m when m.FullPath = f.FullPath ->
                    merge_trees(m.Children, f.Children.[0])
                | Folder _ ->
                    target.Add(item)
            
        for item_group in project_file.ItemGroups do
            for property in item_group.Items do
                if property.ElementName = "Compile" || property.ElementName = "None" || property.ElementName = "EmbeddedResource" then
                    let file_path = normalise_path(Path.Combine(project_containing_folder, property.Include))
                    if is_subdirectory(project_containing_folder, file_path) then
                        merge_trees(file_tree, nested_file(project_containing_folder, file_path, property))
                    else
                        failwithf "'%s' is outside the project folder for '%s'!" file_path project_containing_folder
                        
        {
            Name = name
            FullPath = project_path
            ProjectRootElement = project_file
            Children = file_tree
        }
    
    let read_solution_file(solution_path: string) : Solution =
        let solution_file = SolutionFile.Parse(solution_path)
        
        let projects_list = ResizeArray()
        
        for project in solution_file.ProjectsInOrder do
            if File.Exists(project.AbsolutePath) then
                projects_list.Add(read_project_file(project.ProjectName, project.AbsolutePath))
            else
                printfn "'%s' could not be found!" project.AbsolutePath
        
        {
            Name = Path.GetFileNameWithoutExtension(solution_path)
            FullPath = solution_path
            SolutionFile = solution_file
            Projects = projects_list
        }
        
module TreeOperations =
    
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