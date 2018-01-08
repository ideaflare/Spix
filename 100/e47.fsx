#load "../lib/prime.fsx"

let distinctFactors n = Prime.factors n |> Set.ofList |> Set.count

let consecutiveWithSameDistinct n =
    {2L .. System.Int64.MaxValue }
    |> Seq.windowed n
    |> Seq.find (fun window -> window |> Seq.forall (fun i -> n = distinctFactors i))

#time
printfn "real = %A" (consecutiveWithSameDistinct 4 |> Seq.head)