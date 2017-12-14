let isAbundantNumber n =
    seq {1..(n/2)}
    |> Seq.filter (fun i -> n % i = 0)
    |> Seq.sum > n
   
let rec findComposableNumbers abundantNumbers acc =
    match abundantNumbers with
    | [] -> acc |> List.distinct
    | smallestAbundent :: largerAbundants as abundants ->
        let abundantSums = 
            abundants
            |> List.map (fun largerAbundant -> largerAbundant + smallestAbundent)
        findComposableNumbers largerAbundants (abundantSums @ acc)

let analysisMax = 28123

let composablesUnderMaxAnalysis =
    let abundantNumbers = [1..analysisMax] |> List.filter isAbundantNumber
    findComposableNumbers abundantNumbers []
    |> Set.ofList

let sumComposablesUnderAnalysis =
    (Set.ofList [1..analysisMax]) - composablesUnderMaxAnalysis
    |> Seq.sum

printfn "real = %A" (sumComposablesUnderAnalysis)