namespace FSLN

open System
open System.IO
open Microsoft.Build.Construction
open FSLN

module Path =

    let normalise (path: string) : string =
        Uri(Path.GetFullPath(path)).LocalPath.Replace('\\', Path.AltDirectorySeparatorChar)

    let get_directory_name (path: string) : string =
        Path.GetDirectoryName(path).Replace('\\', Path.AltDirectorySeparatorChar)


module SolutionLoader =

    let read_project_file (name: string, project_path: string) : Project =
        let project_path = Path.normalise(project_path)
        let project_containing_folder = Path.get_directory_name(project_path)
        let project_file = ProjectRootElement.Open(project_path)

        let project =
            {
                Name = name
                FullPath = project_path
                ProjectRootElement = project_file
                Children = ResizeArray<FileTreeEntry>()
            }

        let is_subdirectory (parent_path: string, child_path: string) =
            let parent_path =
                if Path.EndsInDirectorySeparator(parent_path) then
                    parent_path
                else
                    parent_path + Path.AltDirectorySeparatorChar.ToString()

            let child_path =
                if Path.EndsInDirectorySeparator(child_path) then
                    child_path
                else
                    child_path + Path.AltDirectorySeparatorChar.ToString()

            child_path <> parent_path && child_path.StartsWith(parent_path)

        let create_folder (target: Parent, folder_name: string) =
            let parent_path =
                match target with
                | Parent.Project _ -> project_containing_folder
                | Parent.Folder folder -> folder.FullPath

            let new_folder_path =
                Path.Combine(parent_path, folder_name).Replace('\\', Path.AltDirectorySeparatorChar)

            {
                Name = folder_name
                FullPath = new_folder_path
                Children = ResizeArray()
                Parent = target
            }

        let rec merge_trees
            (
                target: Parent,
                remaining_segments: string list,
                file_name: string,
                file_path: string,
                element: ProjectItemElement
            ) =
            match remaining_segments with
            | [] ->
                target.Children.Add(
                    File
                        {
                            Name = file_name
                            FullPath = file_path
                            ProjectItemElement = element
                            Parent = target
                        }
                )
            | folder :: remaining when target.Children.Count > 0 ->
                let last = target.Children.[target.Children.Count - 1]

                match last with
                | Folder merge_folder when merge_folder.Name = folder ->
                    merge_trees(Parent.Folder(merge_folder), remaining, file_name, file_path, element)
                | _ ->
                    let new_folder = create_folder(target, folder)
                    target.Children.Add(Folder new_folder)
                    merge_trees(Parent.Folder(new_folder), remaining, file_name, file_path, element)
            | folder :: remaining ->
                let new_folder = create_folder(target, folder)
                target.Children.Add(Folder new_folder)
                merge_trees(Parent.Folder(new_folder), remaining, file_name, file_path, element)

        for item_group in project_file.ItemGroups do
            for property in item_group.Items do
                if
                    property.ElementName = "Compile"
                    || property.ElementName = "None"
                    || property.ElementName = "EmbeddedResource"
                then
                    let file_path =
                        Path.normalise(Path.Combine(project_containing_folder, property.Include))

                    if is_subdirectory(project_containing_folder, file_path) then
                        let relative_path_segments =
                            Path
                                .get_directory_name(file_path)
                                .Replace(project_containing_folder, "")
                                .Split(Path.AltDirectorySeparatorChar, StringSplitOptions.RemoveEmptyEntries)
                            |> List.ofArray

                        let file_name = Path.GetFileName(file_path)
                        merge_trees(Parent.Project(project), relative_path_segments, file_name, file_path, property)
                    else
                        printfn "'%s' is outside the project folder for '%s'!" file_path project_containing_folder

        project

    let read_solution_file (solution_path: string) : Solution =
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
