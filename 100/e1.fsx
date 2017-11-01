let sumBelow n =
    [1.. (n - 1)]
    |> List.filter (fun n -> n % 3 = 0 || n % 5 = 0)
    |> List.sum

printfn "test 10 (23) = %A" (sumBelow 10)
printfn "real = %A" (sumBelow 1000)