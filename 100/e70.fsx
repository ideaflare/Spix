#load "../lib/prime.fsx"

let maxProduct = 1e7 |> int64

let pairsBelowMax =
    let maxRoot = maxProduct |> float |> sqrt |> ceil |> int64
    Prime.sequence
    |> Seq.takeWhile (fun p -> p <= maxRoot)
    |> Seq.collect (fun p1 ->
        Prime.sequence
        |> Seq.map (fun p2 -> p1 * p2, p1, p2)
        |> Seq.takeWhile (fun (prod, _, _) -> prod <= maxProduct)
    )

let permutation a b =
    let aTxt = string a |> Seq.sort |> Seq.toList
    let bTxt = string b |> Seq.sort |> Seq.toList
    aTxt = bTxt

let pairPhi a b = (a - 1L) * (b - 1L)

let pairWithPermutationPhi (product,a,b) =
    let phi = pairPhi a b
    if permutation product phi
    then Some (product,phi) else None

let ratio ((n,phi) : int64 * int64) : decimal =
    (decimal n) / (decimal phi)

let minNByPhiRatio =
    pairsBelowMax
    |> Seq.choose pairWithPermutationPhi
    |> Seq.sortByDescending ratio
    |> Seq.last
    |> fst

printfn "real = %i" minNByPhiRatio

