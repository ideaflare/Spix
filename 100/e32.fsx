let rec digits = function
    | digit when digit < 10 -> [digit]
    | aboveTen ->
        let aboveWithoutLastDigit = aboveTen / 10
        let digit = aboveTen - (aboveWithoutLastDigit * 10)
        digit :: (digits aboveWithoutLastDigit)

let pandigital (a,b,product) =
    digits a @ digits b @ digits product
    |> List.sort = [1..9]

printfn "test pandigital (12,34,5678)) = %A" (pandigital (12,34,56789))

let distinctSum =
    [2..987]
    |> Seq.collect (fun a -> [2..9876] |> List.map (fun b -> (a,b,a*b)))
    |> Seq.filter pandigital
    |> Seq.map (fun (_,_,product) -> product)
    |> Seq.distinct
    |> Seq.sum

printfn "real = %A" distinctSum
