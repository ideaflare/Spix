let nextChainElement n =
    match n with
    | even when n % 2I = 0I -> even / 2I
    | odd -> (3I * odd) + 1I

let maxByCollatzChain searchRange =
    let lookup = System.Collections.Concurrent.ConcurrentDictionary()
    lookup.GetOrAdd(1I,1I) |> ignore
    let rec newCollatzLength i =
        1I + lookup.GetOrAdd(nextChainElement i, newCollatzLength)        
    let collatzLength x = lookup.GetOrAdd(x, newCollatzLength)
    searchRange
    |> List.maxBy collatzLength

printfn "test 13 (10) = %A" (maxByCollatzChain [13I])
printfn "real = %A" (maxByCollatzChain [2I..999999I])