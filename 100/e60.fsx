#load "../lib/prime.fsx"

let primes = Prime.sequence |> Seq.takeWhile (fun p -> p < 10000L) |> List.ofSeq

let concatPrime a b = 
    let sA = string a
    let sB = string b
    Prime.isPrime (int64 (sA + sB))
    && Prime.isPrime (int64 (sB + sA))

let concatGroups =
    let pSkip n = primes |> Seq.skipWhile (fun p -> p < n)
    seq {
        for pa in primes do
        for pb in pSkip pa do
        if concatPrime pa pb then
            for pc in pSkip pb do
            if concatPrime pc pa && concatPrime pc pb then
                for pd in pSkip pc do
                if concatPrime pd pa && concatPrime pd pb && concatPrime pd pc then
                    for pe in pSkip pd do
                    if concatPrime pe pa && concatPrime pe pb && concatPrime pe pc && concatPrime pe pd then yield [pa;pb;pc;pd;pe]
    }

printfn "real = %A" (concatGroups |> Seq.head |> List.sum)

// Less imperative approach, but 10x slower :

let getMatches group =
    primes
    |> Seq.skipWhile (fun p -> p < (List.last group))
    |> Seq.filter (fun p -> List.forall (fun a -> concatPrime a p) group)
    |> Seq.map (fun pair -> pair :: group)

let concatGroups2 =
    primes
    |> Seq.collect (fun p -> getMatches [p])
    |> Seq.collect getMatches
    |> Seq.collect getMatches
    |> Seq.collect getMatches

printfn "real(v2) = %A" (concatGroups2 |> Seq.head |> List.sum)