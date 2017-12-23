let rec combinations coins (amount : int) : int list list =
    match coins with
    | [] -> []
    | h :: t ->
        let headCombinations =
            let remainder = amount - h
            if remainder = 0 then [[h]]
            else if remainder >= 0 then
                combinations coins remainder
                |> List.map (fun subCombo -> h :: subCombo)
            else []
        let tailCombinations = combinations t amount
        headCombinations @ tailCombinations


printfn "nothingIsEmpty %b" (combinations [] 0 = [])
printfn "coinsNoAmoutIsEmpty %b" (combinations [1..2] 0 = [])
printfn "singleSameAmoutIsSameCoin %b" (combinations [1] 1 = [[1]])
printfn "bigCoinsIgnored %b" (combinations [4;1;2] 1 = [[1]])
printfn "multipleCoinsForAmout %b" (combinations [1] 2 = [[1;1]])
printfn "mixMultipleCoins %b" (combinations [2;3] 5 = [[2;3]])
printfn "differentCoinCombinations %b" (combinations [1;2] 4 = [[1;1;1;1];[1;1;2];[2;2]])

printfn "real = %A" (combinations [1;2;5;10;20;50;100;200] 200 |> List.length)