#load "../lib/numeral.fsx"

// open Microsoft.FSharp.Core.Operators.Checked // not required as 9! < System.Int32.MaxValue
let fact n = [1..(max 1 n)] |> List.reduce (*)

let equalsSumOfDigitFactorials n =
    Numeral.digits n
    |> List.sumBy fact = n

let allFactorialDigitEquals =
    [10..(fact 9)]
    |> List.filter equalsSumOfDigitFactorials
    |> List.sum

printfn "test equalsSumOfDigitFactorials 145 (true) = %A" (equalsSumOfDigitFactorials 145)
printfn "real = %A" allFactorialDigitEquals
