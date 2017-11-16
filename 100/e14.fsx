let nextChainElement n =
    match n with
    | even when n % 2L = 0L -> even / 2L
    | odd -> (3L * odd) + 1L

// ------------------------------------------------------------------------

let maxByCollatzChain searchRange =
    let lookup = System.Collections.Concurrent.ConcurrentDictionary()
    lookup.GetOrAdd(1L,1L) |> ignore
    let rec newCollatzLength i =
        1L + lookup.GetOrAdd(nextChainElement i, newCollatzLength)        
    let collatzLength x = lookup.GetOrAdd(x, newCollatzLength)
    searchRange
    |> List.maxBy collatzLength


printfn "real = %A" (maxByCollatzChain [1L..999999L]) // time 1.87s
// ------------------------------------------------------------------------

let collatzLength n =
    let rec c' n acc =
        1L + match n with
             | 1L -> acc
             | _ -> c' (nextChainElement n) acc
    c' n 0L

printfn "test 13 (10) = %A" (collatzLength 13L)
printfn "real = %A" ([1L..999999L] |> List.maxBy collatzLength) // time 1.61 (seq isn't much faster in this case)

// ------------------------------------------------------------------------

let parallelSerch =
    [|1L..999999L|]
    |> Array.Parallel.map (fun t -> async { return t, collatzLength t })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.reduce (fun (n, collatz) (n2, collatz2) -> if collatz > collatz2 then (n,collatz) else (n2,collatz2) )
    // Slowest, 2.5s (parallel overhead)