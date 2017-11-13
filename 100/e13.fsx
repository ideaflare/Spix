#load "../lib/scriptData.fsx"
let data = ScriptData.dataRows __SOURCE_FILE__ __SOURCE_DIRECTORY__

let firstDigits n =
    data
    |> Seq.map (string >> System.Numerics.BigInteger.Parse)
    |> Seq.reduce (+)
    |> string
    |> (fun i -> i.Substring(0, n))

printfn "real = %A" (firstDigits 10)