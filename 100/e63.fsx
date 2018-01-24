let powDigits n pow =
    System.Numerics.BigInteger.Pow(n,pow)
    |> string
    |> Seq.length

let powerAboveDigits =
    Seq.initInfinite id
    |> Seq.takeWhile (fun pow -> pow <= powDigits 9I pow)
    |> Seq.length

let positiveNDigits =
    [1..powerAboveDigits]
    |> Seq.collect (fun pow -> [1I..9I] |> List.map (fun i -> pow = powDigits i pow))
    |> Seq.filter id
    |> Seq.length

printfn "real = %A" positiveNDigits