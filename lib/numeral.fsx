
open Microsoft.FSharp.Core.Operators.Checked

let digits =
    let rec reversedDigits = function
        | digit when digit < 10 -> [digit]
        | aboveTen ->
            let aboveWithoutLastDigit = aboveTen / 10
            let digit = aboveTen - (aboveWithoutLastDigit * 10)
            digit :: (reversedDigits aboveWithoutLastDigit)
    reversedDigits >> List.rev

let digitListToInt digits = Seq.fold (fun acc d -> (10 * acc) + d) 0 digits

let digitListToBigInt digits =
    let bigDigits = digits |> List.map (fun (i : int) -> System.Numerics.BigInteger(i))
    bigDigits |> Seq.fold (fun acc d -> (10I * acc) + d) 0I
