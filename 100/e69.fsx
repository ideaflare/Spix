#load "../lib/prime.fsx"
let maxCompositeBelow n =
    let rec bp' prod i =
        let nextProd = prod * (Prime.nth i)
        if nextProd > n then prod
        else bp' nextProd (i + 1)
    bp' 2L 2

printfn "test bigPrimeUnder 10 (6) = %A" (maxCompositeBelow 10L)
printfn "real = %A" (maxCompositeBelow 1_000_000L)