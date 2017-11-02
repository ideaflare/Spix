#load "../lib/fibonacci.fsx"

let actual = Fibonacci.sequence |> Seq.take 10 |> List.ofSeq 
let expected = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]

printfn "First 10 fibonacci numbers : expected %A actual %A -> %A" expected actual (expected = actual)