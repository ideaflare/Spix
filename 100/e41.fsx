#load "../lib/numeral.fsx"
#load "../lib/combinatorics.fsx"

let isPrime n =
    if n = 2 then true else
    if n % 2 = 0 then false else
    let root = sqrt (float n) |> int
    {3 .. 2 .. root}
    |> Seq.forall (fun test -> n % test <> 0)

let pandigitals =
    {2..9}
    |> Seq.collect (fun n -> Combinatorics.permutations [1..n])
    |> Seq.map Numeral.digitListToInt

let pandigitalLargePrime =
    pandigitals
    |> Seq.filter isPrime
    |> Seq.max

printfn "test isPandigitalPrime 2143  (true) = %A" (isPrime 2143)

printfn "real = %A" pandigitalLargePrime