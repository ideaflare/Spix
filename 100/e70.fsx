#load "../lib/prime.fsx"

let maxProduct = 1e7 |> int64

let maxRoot = maxProduct |> float |> sqrt |> ceil |> int64

let primesBelowMaxRoot =
    Prime.sequence
    |> Seq.takeWhile (fun p -> p <= maxRoot)

let pairsBelowMax =
    primesBelowMaxRoot
    |> Seq.collect (fun p1 ->
        primesBelowMaxRoot
        |> Seq.skipWhile (fun p2 -> p2 <= p1)
        |> Seq.map (fun p2 -> (p1,p2))
    )

let pairPhi a b = (a - 1L) * (b - 1L)
let permutation a b =
    let aTxt = string a |> Seq.sort |> Seq.toList
    let bTxt = string b |> Seq.sort |> Seq.toList
    aTxt = bTxt

let pairPhiIsPermutatoin (a,b) =
    let product = a * b
    let phi = pairPhi a b
    if permutation product phi
    then Some (product,phi) else None

let ratio (n,phi) = (float n) / (float phi)

let minNByPhiRatio =
    pairsBelowMax
    |> Seq.choose pairPhiIsPermutatoin
    |> Seq.minBy ratio
    |> fst

printfn "real = %A" minNByPhiRatio

