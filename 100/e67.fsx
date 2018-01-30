#load "e18.fsx"

let data = ScriptData.dataRows __SOURCE_FILE__ __SOURCE_DIRECTORY__

let upsideDownTriangle = ScriptData.parseGrid data |> List.rev

printfn "real = %A" (E18.sumMaxParentsDown upsideDownTriangle)