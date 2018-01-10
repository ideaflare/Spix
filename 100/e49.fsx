#load "../lib/prime.fsx"
#load "../lib/numeral.fsx"

let fourDigitPrimes =
    Prime.sequence
    |> Seq.skipWhile (fun p -> p < 1000L) 
    |> Seq.takeWhile (fun p -> p < 10000L)
    |> List.ofSeq

let permutationGroups =
    fourDigitPrimes
    |> Seq.groupBy (fun p -> p |> (int >> Numeral.digits) |> List.sort)
    |> Seq.filter (fun (_,members) -> members |> Seq.length > 2)

let equiDistantMember (items : int64 list) =
    let len = Seq.length items
    seq {
        for b in 0..(len - 1) do
        for a in 0..(b - 1) do
        yield (items.[a],items.[b])
    } |> Seq.filter (fun (a,b) -> 
        let diff = b - a
        items |> List.contains (b + diff)
    ) |> Seq.map (fun (a,b) -> [a;b; b + (b - a)])

let equiDistantTriple =
    permutationGroups
    |> Seq.map (fun (_, members) -> List.ofSeq members)
    |> Seq.collect equiDistantMember
    |> Seq.filter (List.contains 1487L >> not)
    |> List.ofSeq
    |> List.head

let concatNumber (triple : int64 list) = triple |> List.fold (fun acc n -> acc + (string n)) "" |> int64

printfn "real = %A" (concatNumber equiDistantTriple)