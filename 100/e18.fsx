#load "../lib/scriptData.fsx"
let data = ScriptData.dataRows __SOURCE_FILE__ __SOURCE_DIRECTORY__

let splitToCells (txt : string) =
    txt.Split [|' '|]
    |> List.ofArray
    |> List.map int

let grid = data |> List.map splitToCells

let upsideDownTriangle = grid |> List.rev

let rec sumMaxParentsDown (upsideDownTriangle: int list list) =
    match upsideDownTriangle with
    | [[maxPathSum]] -> maxPathSum
    | topRow :: children :: restOfTriangle ->
        let childrenAddMaxParent =
            topRow
            |> List.windowed 2
            |> List.map (List.max)
            |> List.zip children
            |> List.map (fun (child, maxParent) -> child + maxParent)
        sumMaxParentsDown (childrenAddMaxParent :: restOfTriangle)
    | _ -> 0

printfn "real = %A" (sumMaxParentsDown upsideDownTriangle)