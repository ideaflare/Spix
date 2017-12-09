#load "../lib/scriptData.fsx"
let data = ScriptData.dataRows __SOURCE_FILE__ __SOURCE_DIRECTORY__

let splitToCells (txt : string) =
    txt.Split [|' '|]
    |> List.ofArray
    |> List.map int

let grid = data |> List.map splitToCells

let upsideDownTriangle = grid |> List.rev

let rec sumUpwards (triangle: int list list) =
    match triangle with
    | [[top]] -> top
    | readRow :: addRow :: rest ->
        let maxParents =
            readRow
            |> List.windowed 2
            |> List.map (List.max)
        let addedTopRow =
            maxParents
            |> List.zip addRow
            |> List.map (fun (a,b) -> a + b)
        sumUpwards (addedTopRow :: rest)
    | _ -> 0   

printfn "real = %A" (sumUpwards upsideDownTriangle)