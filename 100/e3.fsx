#load "../lib/prime.fsx"

let largestFactor n =
    Prime.factors n
    |> List.max

printfn "test 13195 (29) = %A" (largestFactor 13195L)
printfn "real = %A" (largestFactor 600851475143L)