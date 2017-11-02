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

let isPrime n = sequence |> Seq.find (fun p -> p >= n) = n

// perf could be made much better here if needed
let factors n =
    let rec f remainder primes =
        match isPrime remainder with
        | true -> remainder :: primes
        | false ->            
            let factor = sequence |> Seq.find (fun p -> remainder % p = 0L)
            f (remainder / factor) (factor :: primes)
    f n [] |> List.rev
