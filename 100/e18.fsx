#load "../lib/scriptData.fsx"
let data = ScriptData.dataRows __SOURCE_FILE__ __SOURCE_DIRECTORY__

let upsideDownTriangle = ScriptData.parseGrid data |> List.rev

let sumMaxParentsDown (upsideDownTriangle: int list list) =
    let childrenAddMaxParent parents children =
            parents
            |> List.windowed 2
            |> List.map (List.max)
            |> List.zip children
            |> List.map (fun (child, maxParent) -> child + maxParent)
    upsideDownTriangle
    |> List.reduce childrenAddMaxParent
    |> List.sum

printfn "real = %A" (sumMaxParentsDown upsideDownTriangle)