let findSmallestMultiple max =
    let range =
        [1 .. max]
        |> List.rev
        |> List.map System.Numerics.BigInteger
    let product = range |> List.reduce (*)
    let isValidMultiple n = range |> List.forall (fun p -> n % p = 0I)
    let sameOrSmaller n div =
        match n / div with
        | s when isValidMultiple s -> s
        | _ -> n    
    List.fold sameOrSmaller product range

printfn "test 10 (2520) = %A" (findSmallestMultiple 10)
printfn "real = %A" (findSmallestMultiple 20)