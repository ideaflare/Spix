let nextChainElement n =
    match n with
    | even when n % 2I = 0I -> even / 2I
    | odd -> (3I * odd) + 1I
    
let memoize (f: 'a -> 'b) =
    let lookup = System.Collections.Concurrent.ConcurrentDictionary<'a,'b>()
    fun n -> lookup.GetOrAdd(n,f)

let collatzChainLength n =
    let lookup = System.Collections.Concurrent.ConcurrentDictionary()
    lookup.GetOrAdd(1I,1I) |> ignore
    let rec getCollatzLength n =
        let nextChainItem = nextChainElement n
        1I + lookup.GetOrAdd(nextChainItem, getCollatzLength)        
    lookup.GetOrAdd(n, getCollatzLength)

let longestChainUnder n =
    [1I..n]
    |> List.rev
    |> List.skip 1
    |> List.map collatzChainLength
    |> List.max


printfn "test 13 (10) = %A" (collatzChainLength 13I)
printfn "real = %A" (longestChainUnder 999999I)