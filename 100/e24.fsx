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


let t0 = permutations ([] : int list)
let t1 = permutations [1]
let t2 = permutations [1;2]
let t2expected = [[1; 2]; [2; 1]]
let t3 = permutations [1..3]
let t3expected = [[1; 2; 3]; [1; 3; 2]; [2; 1; 3]; [2; 3; 1]; [3; 1; 2]; [3; 2; 1]]

printfn "test [] emtpy = %A [%b]" t0 (Seq.isEmpty t0)
printfn "test [1] single = %A [%b]" t1 (List.ofSeq t1 = [[1]])
printfn "test [1;2] 2! combinations = %A [%b]" t2 (List.ofSeq t2 = t2expected)
printfn "test [1..3] 3! combinations = %A [%b]" t3 (List.ofSeq t3 = t3expected)

let millionth = 
    permutations [0..9]
    |> Seq.skip 999999
    |> Seq.head
    |> List.map (string)
    |> List.reduce (+);;

printfn "real = %A" millionth