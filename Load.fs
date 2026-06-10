namespace fsln

open System
open System.IO
open Microsoft.Build.Construction
open fsln

module SolutionLoader =
    
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