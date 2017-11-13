#load "../lib/prime.fsx"

let ``factors`` n expected =
    let actual = Prime.factors n
    printfn "Factorize %d : expected %A actual %A -> %A" n expected actual (expected = actual)

let ``nth`` () =
    let actual = Prime.nth 1, Prime.nth 2
    let expected = 2L, 3L
    printfn "nth 1 & 2 : expected %A actual %A -> %A" expected actual (expected = actual)

``factors`` 4L [2L; 2L]
``factors`` 9L [3L; 3L]
``factors`` 100L [2L; 2L; 5L; 5L]
``factors`` 13195L [5L; 7L; 13L; 29L]
``nth`` ()

#time "on"
Prime.factors 98765L;;