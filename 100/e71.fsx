#load "../lib/rational.fsx"
open Rational

let leftOf3over7 denominatorMax =
    let t = 3I /. 7I
    { 2I .. denominatorMax }
    |> Seq.map (fun d -> ((t.Numerator * d) / t.Denominator) /. d)
    |> Seq.distinct
    |> Seq.filter (fun r -> (compare r t) <> 0)
    |> Seq.sortWith compare
    |> (Seq.last >> (fun r -> r.Numerator))

printfn "test 8 (2) = %A" (leftOf3over7 8I)
printfn "real = %A" (leftOf3over7 (1e6 |> Big))