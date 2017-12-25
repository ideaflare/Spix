let digits =
    let rec reversedDigits = function
        | digit when digit < 10 -> [digit]
        | aboveTen ->
            let aboveWithoutLastDigit = aboveTen / 10
            let digit = aboveTen - (aboveWithoutLastDigit * 10)
            digit :: (reversedDigits aboveWithoutLastDigit)
    reversedDigits >> List.rev