let rec fact n = List.fold (*) 1I [1I..n]

let combinatoricOptions n r = fact n / (fact r * (fact (n - r)))

let totalCombinations =
    [1I..100I]
    |> List.collect (fun n -> [1I..n] |> List.map (fun r -> combinatoricOptions n r))
    |> List.filter (fun i -> i > 1000000I)
    |> List.length

printfn "real = %A" totalCombinations