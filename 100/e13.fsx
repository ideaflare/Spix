#load "../lib/scriptData.fsx"
let data = ScriptData.dataRows __SOURCE_FILE__ __SOURCE_DIRECTORY__

let firstDigits n =
    data
    |> Seq.map (string >> System.Numerics.BigInteger.Parse)
    |> Seq.reduce (+)
    |> string
    |> Seq.take n
    |> Seq.toArray
    |> (fun chars -> System.String(chars))

printfn "real = %A" (firstDigits 10)