#load "../lib/prime.fsx"

let truncateRight n =
    let rec t' num acc =
        match num with
        | i when i < 10 -> acc
        | n ->
            let ns = n / 10
            t' ns (ns :: acc)
    t' n [] |> List.rev

let (^^) a b =
    List.init b (fun _ -> a)
    |> List.fold (*) 1

let truncateLeft rightParts n =
    let rightr main sub idx = 
        let remain = sub * (10 ^^ (idx + 1))
        main - remain
    rightParts
    |> List.mapi (fun idx sub -> rightr n sub idx)

let primes = Prime.sequence |> Seq.take 1000000 |> Seq.map int |> Set.ofSeq

let isPrime = primes.Contains

let bothTruncationsPrime n =
    let rightParts = truncateRight n
    let leftParts = truncateLeft rightParts n
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