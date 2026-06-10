namespace fsln

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
        
    member this.FindFolderAndSiblings(full_path: string) : (FileTreeFolder * ResizeArray<FileTreeEntry>) option =
        let rec search (search_space: ResizeArray<FileTreeEntry>) =
            let mutable found = None
            for entry in search_space do
                match entry with
                | File _ -> ()
                | Folder f when f.FullPath = full_path -> found <- Some (f, search_space)
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