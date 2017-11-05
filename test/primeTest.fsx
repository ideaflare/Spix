#load "../lib/prime.fsx"

let ``factors`` () =
    let actual = Prime.factors 13195L 
    let expected = [5L; 7L; 13L; 29L]
    printfn "Factorize 13195 : expected %A actual %A -> %A" expected actual (expected = actual)

let ``nth`` () =
    let actual = Prime.nth 1, Prime.nth 2
    let expected = 2L, 3L
    printfn "nth 1 & 2 : expected %A actual %A -> %A" expected actual (expected = actual)

``factors`` ()
``nth`` () 