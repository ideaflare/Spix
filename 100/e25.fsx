let fib =
    let rec f' a b = seq {
        yield a
        yield! f' b (a + b)
    }
    f' 0I 1I

let digits (n : System.Numerics.BigInteger) = n.ToString().Length

printfn "test digits 5I (1) = %A" (digits 5I)
printfn "test digits 4321I (4) = %A" (digits 4321I)

let firstFibIndex minDigits =
    fib
    |> Seq.mapi (fun index f -> (index,f))
    |> Seq.skipWhile (fun (_,fib) -> digits fib < minDigits)
    |> Seq.head
    |> (fun (index,_) -> index)

printfn "test 2 (7) = %A" (firstFibIndex 2)
printfn "test 3 (12) = %A" (firstFibIndex 3)
printfn "real = %A" (firstFibIndex 1000)