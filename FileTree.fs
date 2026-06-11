namespace fsln

open Microsoft.Build.Construction

// https://git-scm.com/docs/git-status

[<RequireQualifiedAccess>]
type Parent =
    | Project of Project
    | Folder of FileTreeFolder
    member this.Children =
        match this with
        | Project p -> p.Children
        | Folder f -> f.Children
    member this.AddChild(child: FileTreeEntry) =
        this.Children.Add(child.WithParent(this))

and FileTreeFile =
    {
        Name: string
        FullPath: string
        ProjectItemElement: ProjectItemElement
        Parent: Parent
    }
    
and FileTreeFolder =
    {
        Name: string
        FullPath: string
        Children: ResizeArray<FileTreeEntry>
        Parent: Parent
    }
    
    member this.EnumerateFiles() : FileTreeFile seq =
        seq {
            for f in this.Children do
                match f with
                | File x -> yield x
                | Folder y -> yield! y.EnumerateFiles()
        }
    
and FileTreeEntry =
    | File of FileTreeFile
    | Folder of FileTreeFolder
    member this.Parent =
        match this with
        | File f -> f.Parent
        | Folder f -> f.Parent
    member this.WithParent(parent: Parent) =
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
    
type Solution =
    {
        Name: string
        FullPath: string
        SolutionFile: SolutionFile
        Projects: ResizeArray<Project>
    }