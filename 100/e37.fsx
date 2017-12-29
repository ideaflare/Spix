#load "../lib/prime.fsx"

let truncateRight n =
    let rec t' num acc =
        match num with
        | i when i < 10 -> acc
        | n ->
            let ns = n / 10
            t' ns (ns :: acc)
    t' n []

let truncateLeft n =
    let rec t' acc modDigit =
        match n % modDigit with
        | t when t < n -> t' (t :: acc) (modDigit * 10)
        | _ -> acc
    t' [] 10

let primes = Prime.sequence |> Seq.take 1000000 |> Seq.map int |> Set.ofSeq

let isPrime = primes.Contains

let bothTruncationsPrime n =
    let rightParts = truncateRight n
    let leftParts = truncateLeft n
    Seq.forall isPrime rightParts && Seq.forall isPrime leftParts

printfn "test bothTruncationsPrime 3797 (true) = %A" (bothTruncationsPrime 3797)

let truncatablePrimes n =
    Prime.sequence
    |> Seq.map int
    |> Seq.skipWhile (fun p -> p < 10)
    |> Seq.filter bothTruncationsPrime
    |> Seq.take n
    |> List.ofSeq

printfn "real = %A" (truncatablePrimes 11 |> List.sum)
