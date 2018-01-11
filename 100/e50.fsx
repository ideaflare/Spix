#load "../lib/prime.fsx"

let primesBelow n = Prime.sequence |> Seq.takeWhile (fun p -> p < n) |> List.ofSeq

let maxWindow n =
    let rec mw items sum windowSize =
        match items with
        | [] -> windowSize
        | x :: xs ->
            let newSum = sum + x
            if newSum > n
            then windowSize
            else mw xs newSum (windowSize + 1)
    mw (primesBelow n) 0L 0

let sumPrimesIsPrimeBelow n primes = 
    let primeSum = Seq.sum primes
    primeSum < n && Prime.isPrime primeSum

let longestPrimeSequence n =
    let primes = primesBelow (n / 2L)
    [2..(maxWindow n)]
    |> List.rev
    |> Seq.map (fun i -> 
        let windows = primes |> Seq.windowed i
        windows |> Seq.tryFind (sumPrimesIsPrimeBelow n)
    )
    |> Seq.choose id
    |> Seq.head

let primeSum n = Seq.sum (longestPrimeSequence n)

printfn "test primeSums 100 (41) = %i" (primeSum 100L)
printfn "test primeSums 1000 (953) = %i" (primeSum 1000L)

printfn "real = %i" (primeSum 1000000L)