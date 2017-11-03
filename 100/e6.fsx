let square x = x * x

let sumSquares n =
    [1..n]
    |> List.fold (fun sum i -> sum + (square i)) 0

let squareSum n =
    [1..n]
    |> List.reduce (+)
    |> square

let squareSum_diff_sumSquares n = (squareSum n) - (sumSquares n)

printfn "test 10 (2640) = %A" (squareSum_diff_sumSquares 10)
printfn "real = %A" (squareSum_diff_sumSquares 100)