module Reader

type Code = { 
    Line: string
    LineNumber: int
    Depth: int
}

let toCode (input: string list) = 
    let findDepth line = 
        // depth is count of space
        let rec findDepth' (line: string) (depth: int) = 
            if line.StartsWith(" ") then
                findDepth' (line.Substring(1)) (depth + 1)
            else
                depth
        findDepth' line 0
        
    input
    |> List.mapi (fun i line -> { Line = line.Trim(); LineNumber = i; Depth = findDepth line })


let loadCode path =
    let lines = System.IO.File.ReadAllLines(path) |> List.ofArray
    toCode lines

