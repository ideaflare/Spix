let digitSumOf2Pow n =
    System.Numerics.BigInteger.Pow(2I, n)
    |> string
    |> Seq.sumBy (string >> System.Int32.Parse)

printfn "test 15 (26) = %A" (digitSumOf2Pow 15)
printfn "real = %A" (digitSumOf2Pow 1000)