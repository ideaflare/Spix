#load "../lib/fibonacci.fsx"

let fibSum max =
    Fibonacci.sequence
    |> Seq.skip 2
    |> Seq.takeWhile (fun n -> n < max)
    |> Seq.filter (fun n -> n % 2 = 0)
    |> Seq.sum

printfn "test 89 (44) = %A" (fibSum 89)
printfn "real = %A" (fibSum 4000000)