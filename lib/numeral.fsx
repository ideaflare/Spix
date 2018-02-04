
open Microsoft.FSharp.Core.Operators.Checked

let digits =
    let rec reversedDigits = function
        | digit when digit < 10 -> [digit]
        | aboveTen ->
            let aboveWithoutLastDigit = aboveTen / 10
            let digit = aboveTen - (aboveWithoutLastDigit * 10)
            digit :: (reversedDigits aboveWithoutLastDigit)
    reversedDigits >> List.rev

let concatDigits digits = digits |> Seq.map string |> Seq.fold (+) "0"

let digitListToInt digits = concatDigits digits |> int

let digitListToBigInt digits = digits |> concatDigits |> System.Numerics.BigInteger.Parse