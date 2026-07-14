namespace FSLN

open System.Diagnostics

[<Struct>]
type GitStatusType =
    | Unchanged
    | Modified
    | TypeChanged
    | Added
    | Deleted
    | Renamed
    | Copied
    | Unmerged
    | Untracked

    override this.ToString() : string =
        match this with
        | Unchanged -> "."
        | Modified -> "M"
        | TypeChanged -> "T"
        | Added -> "A"
        | Deleted -> "D"
        | Renamed -> "R"
        | Copied -> "C"
        | Unmerged -> "U"
        | Untracked -> "?"

    static member FromChar(value: char) : GitStatusType =
        match value with
        | ' '
        | '.' -> Unchanged
        | 'M' -> Modified
        | 'T' -> TypeChanged
        | 'A' -> Added
        | 'D' -> Deleted
        | 'R' -> Renamed
        | 'C' -> Copied
        | 'U' -> Unmerged
        | '?' -> Untracked
        | other -> failwithf "Unrecognised GitStatusType '%c'" other

    static member FromString(value: string) : GitStatusType =
        if value.Length <> 1 then
            failwith "Invalid length for GitStatusType"

        GitStatusType.FromChar(value.[0])

[<Struct>]
type GitFileStatus = { Index: GitStatusType; WorkingTree: GitStatusType }

type GitStatus =
    {
        Branch: string
        Upstream: string option
        AheadBehind: (int * int) option
        Files: Map<string, GitFileStatus>
        IndexDirty: int
        WorkingTreeDirty: int
    }

    static member Parse(raw: string) : GitStatus =
        let mutable branch_head = ""
        let mutable branch_upstream: string option = None
        let mutable branch_ab: (int * int) option = None
        let mutable files: Map<string, GitFileStatus> = Map.empty

        let inline parse_header (line: string array) : unit =
            match line.[1] with
            | "branch.head" -> branch_head <- line.[2]
            | "branch.upstream" -> branch_upstream <- Some line.[2]
            | "branch.ab" -> branch_ab <- Some(int(line.[2].Substring(1)), int(line.[3].Substring(1)))
            | _ -> ()

        let inline parse_file (line: string array) : unit =
            let index = GitStatusType.FromChar(line.[1].[0])
            let working_tree = GitStatusType.FromChar(line.[1].[1])
            let file = line.[8]
            files <- files.Add(file, { Index = index; WorkingTree = working_tree })

        let inline parse_line (line: string array) : unit =
            match line.[0] with
            | "#" -> parse_header(line)
            | "1" -> parse_file(line)
            | _ -> ()

        let lines = raw.Split('\u0000')

        for line in lines do
            let line_parts = line.Split(' ')
            parse_line(line_parts)

        {
            Branch = branch_head
            Upstream = branch_upstream
            AheadBehind = branch_ab
            Files = files
            IndexDirty = files.Values |> Seq.map _.Index |> Seq.filter((<>) Unchanged) |> Seq.length
            WorkingTreeDirty = files.Values |> Seq.map _.WorkingTree |> Seq.filter((<>) Unchanged) |> Seq.length
        }

    static member Fetch() : GitStatus option =
        let start_info =
            ProcessStartInfo(
                "git",
                "status --ignore-submodules --no-renames --porcelain=v2 -z -b",
                RedirectStandardOutput = true
            )

        let proc = Process.Start(start_info)
        let output = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        if proc.ExitCode = 0 then Some(GitStatus.Parse(output)) else None
