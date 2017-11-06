#load "../lib/scriptData.fsx"
let data = ScriptData.dataText __SOURCE_FILE__ __SOURCE_DIRECTORY__

let adjacentProduct digits =
    data
    |> Seq.map (string >> System.Numerics.BigInteger.Parse)
    |> Seq.windowed digits
    |> Seq.map (Seq.reduce (*))
    |> Seq.max

printfn "test 4 (5832) = %A" (adjacentProduct 4)
printfn "real = %A" (adjacentProduct 13)