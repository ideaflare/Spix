let recurringCycleLength =
    let rec r' previousDivs numerator denominator =
        let div = numerator / denominator
        let remainder = numerator - (denominator * div)
        if remainder = 0 
        then 0
        else
            let maybePrevDivIndex = previousDivs |> List.tryFindIndex (fun k -> k = numerator)
            match maybePrevDivIndex with
            | Some idx -> idx + 1
            | None -> r' (numerator :: previousDivs) (remainder * 10) denominator
    r' [] 1


printfn "test 2 (0) = %A" (recurringCycleLength 2)
printfn "test 3 (1) = %A" (recurringCycleLength 3)
printfn "test 5 (0) = %A" (recurringCycleLength 5)
printfn "test 6 (1) = %A" (recurringCycleLength 6)
printfn "test 7 (6) = %A" (recurringCycleLength 7)

printfn "real = %A" ([1..999] |> Seq.maxBy recurringCycleLength)