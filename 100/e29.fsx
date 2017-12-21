let term a b = System.Numerics.BigInteger.Pow(a,b)

let distinctTerms aRange bRange =
    aRange
    |> List.collect (fun a -> bRange |> List.map (fun b -> term a b))
    |> List.distinct
    |> List.length

printfn "test 2..5 (15) = %A" (distinctTerms [2I .. 5I] [2 .. 5])

printfn "real = %A" (distinctTerms [2I .. 100I] [2 .. 100])