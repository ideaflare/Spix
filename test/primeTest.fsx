#load "../lib/prime.fsx"

let actual = Prime.factors 13195L 
let expected = [5L; 7L; 13L; 29L]

printfn "Factorize 13195 : expected %A actual %A -> %A" expected actual (expected = actual)