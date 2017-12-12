#load "../lib/combinatorics.fsx"

let countFactorialDigits n =
    Combinatorics.factorial n
    |> string
    |> Seq.sumBy (string >> System.Int32.Parse)

printfn "test 10 (27) = %A" (countFactorialDigits 10I)
printfn "real = %A" (countFactorialDigits 100I)