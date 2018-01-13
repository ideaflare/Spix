#load "../lib/numeral.fsx"

let sortedDigits = Numeral.digits >> List.sort
let sameDigits x y = sortedDigits x = sortedDigits y

printfn "test sameDigits 125874 251748 (true) = %b" (sameDigits 125874 251748)

let sixMultipliersHaveSameDigits n = Seq.forall (fun i -> sameDigits n (i * n)) {1..6}

printfn "real = %A" (Seq.initInfinite id |> Seq.skip 1 |> Seq.find sixMultipliersHaveSameDigits)
