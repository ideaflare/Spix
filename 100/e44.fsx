type Big = System.Numerics.BigInteger

let pentagonal n = n * (3I * n - 1I) / 2I

let first10Pentagonal = List.map pentagonal [1I..10I]
let expectedFirst10 = [1; 5; 12; 22; 35; 51; 70; 92; 117; 145] |> List.map (fun i -> Big(i))

printfn "test first 10 pentagonals (%A)" (first10Pentagonal = expectedFirst10)

let pentagonalSequence =
    Seq.initInfinite (fun i -> Big(i + 1))
    |> Seq.map pentagonal
    |> Seq.cache

let maxSeqGuess = 99999

let isPentagonal =
    let lookup = 
        pentagonalSequence
        |> Seq.take maxSeqGuess
        |> Set.ofSeq
    fun n -> lookup.Contains(n)
   
let sumIsPentagonal a b = a + b |> isPentagonal
let diffIsPentagonal a b = (abs (a - b)) |> isPentagonal

printfn "test 22 and 70 sumPentagonal (%A)" (sumIsPentagonal 22I 70I)

let find pairCondition sequence =
    let rec f' idx =
        let n = sequence |> Seq.skip idx |> Seq.head
        let maybeMatch = 
            sequence 
            |> Seq.takeWhile (fun pair -> pair < n) 
            |> Seq.tryFind (fun p ->
                pairCondition n p)
        match maybeMatch with
        | Some(p) -> (p,n)
        | None when idx < maxSeqGuess -> f' (idx + 1)
        | _ ->
            printfn "[NotFound] Solution not within sequence range %i" maxSeqGuess
            (0I,0I)
    f' 0

printfn "test find a sumPentagonal %A" (find sumIsPentagonal pentagonalSequence)

let diffAndSumPentagonal a b =
    diffIsPentagonal a b
    && sumIsPentagonal a b

let sumAndDiffPentagonalPair = find diffAndSumPentagonal pentagonalSequence

#time
printfn "test find a sumAndDiffPentagonal %A" (sumAndDiffPentagonalPair)

printfn "real = %A" (sumAndDiffPentagonalPair |> (fun (a,b) -> (abs (a - b))))