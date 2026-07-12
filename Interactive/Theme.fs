namespace fsln

open System.Drawing

type TreeConnectors =
    {
        Branch: string
        Leaf: string
        Vertical: string
        Empty: string
    }

type Theme =
    {
        TreeConnectors: TreeConnectors
        IconExpanded: char
        IconCollapsed: char
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
        ColorConnectorsSolution: Color
    }
    
    static member Default =
        {
            TreeConnectors = { Branch = "├─"; Leaf = "└─"; Vertical = "│ "; Empty = "  " }
            IconExpanded = '-'
            IconCollapsed = '+'
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
            ColorConnectorsSolution = Color.FromArgb(0x664488)
        }