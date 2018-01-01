#load "../lib/numeral.fsx"
let digits = Numeral.digits

let sequenceDigits =
    let rec seqDigits' n =
        seq {
            yield! (digits n)
            yield! seqDigits' (n + 1)
        }
    seqDigits' 1

printfn "test sequenceDigits 1  (1) = %A" (sequenceDigits |> Seq.head)
printfn "test sequenceDigits 12 (1) = %A" (sequenceDigits |> Seq.skip 11 |> Seq.take 1)

let tenPow n = 10.0 ** (float n) |> int

let expressionProduct =
    [0..6]
    |> Seq.map (tenPow >> (fun nth -> sequenceDigits |> Seq.item (nth - 1)))
    |> Seq.reduce (*)

printfn "real = %A" expressionProduct