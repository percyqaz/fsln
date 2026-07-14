namespace FSLN

open Microsoft.Build.Construction

[<RequireQualifiedAccess>]
type Parent =
    | Project of Project
    | Folder of FileTreeFolder

    member this.Children =
        match this with
        | Project p -> p.Children
        | Folder f -> f.Children

    member this.AddChild(child: FileTreeEntry) : unit =
        this.Children.Add(child.WithParent(this))

    member this.TryFindFile(name: string) : FileTreeFile option =
        this.Children
        |> Seq.tryPick(
            function
            | FileTreeEntry.File file when file.Name = name -> Some(file)
            | _ -> None
        )

    member this.TryFindFolder(name: string) : FileTreeFolder option =
        this.Children
        |> Seq.tryPick(
            function
            | FileTreeEntry.Folder folder when folder.Name = name -> Some(folder)
            | _ -> None
        )

and FileTreeFile =
    {
        Name: string
        FullPath: string
        ProjectItemElement: ProjectItemElement
        Parent: Parent
    }

    member this.ParentProject: Project =
        match this.Parent with
        | Parent.Folder parent_folder -> parent_folder.ParentProject
        | Parent.Project parent_project -> parent_project

and FileTreeFolder =
    {
        Name: string
        FullPath: string
        Children: ResizeArray<FileTreeEntry>
        Parent: Parent
    }

    member this.EnumerateFiles() : FileTreeFile seq =
        seq {
            for child in this.Children do
                match child with
                | File file -> yield file
                | Folder folder -> yield! folder.EnumerateFiles()
        }

    member this.EnumerateSubfolders() : FileTreeFolder seq =
        seq {
            for child in this.Children do
                match child with
                | File _ -> ()
                | Folder folder ->
                    yield folder
                    yield! folder.EnumerateSubfolders()
        }

    member this.ParentProject: Project =
        match this.Parent with
        | Parent.Folder parent_folder -> parent_folder.ParentProject
        | Parent.Project parent_project -> parent_project

and FileTreeEntry =
    | File of FileTreeFile
    | Folder of FileTreeFolder

    member this.Parent =
        match this with
        | File f -> f.Parent
        | Folder f -> f.Parent

    member this.WithParent(parent: Parent) : FileTreeEntry =
        match this with
        | File f -> File { f with Parent = parent }
        | Folder f -> Folder { f with Parent = parent }

and Project =
    {
        Name: string
        FullPath: string
        ProjectRootElement: ProjectRootElement
        Children: ResizeArray<FileTreeEntry>
    }

    member this.EnumerateFiles() : FileTreeFile seq =
        seq {
            for child in this.Children do
                match child with
                | File file -> yield file
                | Folder folder -> yield! folder.EnumerateFiles()
        }

    member this.EnumerateSubfolders() : FileTreeFolder seq =
        seq {
            for child in this.Children do
                match child with
                | File _ -> ()
                | Folder folder ->
                    yield folder
                    yield! folder.EnumerateSubfolders()
        }

type Solution =
    {
        Name: string
        FullPath: string
        SolutionFile: SolutionFile
        Projects: ResizeArray<Project>
    }
