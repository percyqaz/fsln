namespace fsln

open System.Drawing

type TreeConnectors =
    {
        Branch: string
        Leaf: string
        Vertical: string
        Empty: string
    }

    static member Parse(value: string) : Result<TreeConnectors, string> =
        let split = value.Split(",")

        if split.Length <> 4 then
            Error "Tree connectors must be 4 values"
        else

        let l = split.[0].Length

        if split.[1].Length <> l || split.[2].Length <> l || split.[3].Length <> l then
            Error "Tree connectors must all be the same length (1 or 2 characters recommended)"
        else

        Ok
            {
                Branch = split.[0]
                Leaf = split.[1]
                Vertical = split.[2]
                Empty = split.[3]
            }

type ExpandMarkers =
    {
        Open: string
        Closed: string
    }

    static member Parse(value: string) : Result<ExpandMarkers, string> =
        let split = value.Split(",")

        if split.Length <> 2 then
            Error "Expand markers must be 2 values"
        else if

            split.[1].Length <> split.[0].Length
        then
            Error "Expand markers must be the same length (1 or 2 characters recommended)"
        else

        Ok { Open = split.[0]; Closed = split.[1] }

type Theme =
    {
        TreeConnectors: TreeConnectors
        ExpandMarkers: ExpandMarkers
        IconFile: char
        IconFolder: char
        IconProject: char
        IconSolution: char
        ColorExpandIcon: Color
        ColorFile: Color
        ColorFolder: Color
        ColorProject: Color
        ColorSolution: Color
        ColorSelection: Color
        ColorConnectorsDefault: Color
        ColorConnectorsFolder: Color
        ColorConnectorsProject: Color
    }

    static member Default =
        {
            TreeConnectors =
                {
                    Branch = "├─"
                    Leaf = "└─"
                    Vertical = "│ "
                    Empty = "  "
                }
            ExpandMarkers = { Open = "-"; Closed = "+" }
            IconFile = '*'
            IconFolder = '■'
            IconProject = '■'
            IconSolution = '■'
            ColorExpandIcon = Color.FromArgb(0x444488)
            ColorFile = Color.FromArgb(0xdddddd)
            ColorFolder = Color.FromArgb(0xffff66)
            ColorProject = Color.FromArgb(0xdd00ff)
            ColorSolution = Color.FromArgb(0xaa99ff)
            ColorSelection = Color.FromArgb(0x333300)
            ColorConnectorsDefault = Color.FromArgb(0x222222)
            ColorConnectorsFolder = Color.FromArgb(0x888844)
            ColorConnectorsProject = Color.FromArgb(0x664488)
        }

    member this.Set(key: string, value: string) : Result<Theme, string> =

        let inline parse_icon () : Result<char, string> =
            if value.Length <> 1 then Error "Value must be 1 character" else Ok value.[0]

        let inline parse_color () : Result<Color, string> =
            try
                Ok(ColorTranslator.FromHtml(value))
            with err ->
                Error err.Message

        let inline p (parse: unit -> Result<'T, string>) (set: 'T -> Theme) : Result<Theme, string> =
            match parse() with
            | Ok v -> Ok(set v)
            | Error reason -> Error reason

        match key with
        | "tree_connectors" -> p (fun () -> TreeConnectors.Parse(value)) (fun v -> { this with TreeConnectors = v })
        | "expand_markers" -> p (fun () -> ExpandMarkers.Parse(value)) (fun v -> { this with ExpandMarkers = v })
        | "icon_file" -> p parse_icon (fun v -> { this with IconFile = v })
        | "icon_folder" -> p parse_icon (fun v -> { this with IconFolder = v })
        | "icon_project" -> p parse_icon (fun v -> { this with IconProject = v })
        | "icon_solution" -> p parse_icon (fun v -> { this with IconSolution = v })
        | "color_expand_icon" -> p parse_color (fun v -> { this with ColorExpandIcon = v })
        | "color_file" -> p parse_color (fun v -> { this with ColorFile = v })
        | "color_folder" -> p parse_color (fun v -> { this with ColorFolder = v })
        | "color_project" -> p parse_color (fun v -> { this with ColorProject = v })
        | "color_solution" -> p parse_color (fun v -> { this with ColorSolution = v })
        | "color_selection" -> p parse_color (fun v -> { this with ColorSelection = v })
        | "color_connectors_default" -> p parse_color (fun v -> { this with ColorConnectorsDefault = v })
        | "color_connectors_folder" -> p parse_color (fun v -> { this with ColorConnectorsFolder = v })
        | "color_connectors_project" -> p parse_color (fun v -> { this with ColorConnectorsProject = v })
        | _ -> Error(sprintf "Unrecognised setting '%s'" key)
