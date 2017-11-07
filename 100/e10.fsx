#load "../lib/prime.fsx"

let primesBelow n =
    Prime.sequence
    |> Seq.takeWhile (fun p -> p < n)
    |> Seq.sum

printfn "test 10 (17) = %A" (primesBelow 10L)
printfn "real = %A" (primesBelow 2000000L)