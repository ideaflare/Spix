module private PrimeCache =
    type SieveTable = System.Collections.Generic.Dictionary<int64, int64 list>
    let addOrUpdatePrime (lookup: SieveTable) nextLookup prime =
        if(lookup.ContainsKey(nextLookup))
            then lookup.[nextLookup] <-  prime :: lookup.[nextLookup]
            else lookup.[nextLookup] <- [prime]
    let movePrimeToNextLookup (lookup: SieveTable) number prime = 
        let nextLookup = prime + number
        addOrUpdatePrime lookup nextLookup prime      
    let isPrime (lookup:SieveTable) number =
        if(lookup.ContainsKey(number))
            then
                let primes = lookup.[number]
                primes |> Seq.iter (movePrimeToNextLookup lookup number)
                lookup.Remove(number) |> ignore
                false
            else 
                let primeSquare = number * number
                lookup.Add(primeSquare, [number])
                true

// Improvements: generic PrimeCache -> intSequence & bigSequence (no max, uses 2I for 1st prime)
//               relation between isPrime & factors -> help perf ? less functions ?

let sequence =
    let lookup = PrimeCache.SieveTable()
    let rec findPrime n =
        if (PrimeCache.isPrime lookup n) then n
        else findPrime (n + 1L)
    let rec primesFrom n = seq {
        let prime = findPrime n        
        yield prime
        yield! primesFrom (prime + 1L)       
    }
    primesFrom 2L
    |> Seq.takeWhile (fun p -> p <= System.Int64.MaxValue)
    |> Seq.cache

let private primeMap = System.Collections.Concurrent.ConcurrentDictionary<int, int64>()

let nth n =
    let ithPrime i = sequence |> Seq.skip (i - 1) |> Seq.head
    primeMap.GetOrAdd(n, ithPrime)

let factors n =
    let rec f remainder prime factors =
        let nthPrime = nth prime
        // perf could be better, eg only check with primes <= sqrt of remainder
        // printfn "factors: %A remainder: %A prime %A:%A" factors remainder prime nthPrime
        match remainder ,(remainder % nthPrime) with
        | 1L, _ -> factors
        | _, 0L -> f (remainder / nthPrime) prime (nthPrime :: factors)
        | _ -> f remainder (prime + 1) factors
    f n 0 [] |> List.rev