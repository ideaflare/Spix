let factorial n =
    let rec f' n product =
        match n with
        | fin when fin < 2I -> product
        | _ -> f' (n - 1I) (product * n)
    f' n 1I

let rec permutations items =
    seq {
        match items with
        | [x] -> yield [x]
        | xs -> yield! xs |> Seq.collect (fun x ->
                permutations (List.except [x] xs)
                |> Seq.map (fun p -> x :: p))
        }

let rec exhaustivePermutations = function
    | [x] -> [[x]]
    | xs -> xs |> List.collect (fun x ->
            exhaustivePermutations (List.except [x] xs)
            |> List.map (fun p -> x :: p))