let spiralCount spiralsize =
    let edges increment =
        let previousEdge = (increment - 1) * (increment - 1)
        previousEdge * 4 + increment * 10
    let finalIncrement = spiralsize - 1
    let edgesSum = [2 .. 2 .. finalIncrement] |> List.sumBy edges
    1 + edgesSum
    
printfn "test 1 (1) = %A" (spiralCount 1)
printfn "test 3 (2) = %A" (spiralCount 3)
printfn "test 5 (6) = %A" (spiralCount 5)

printfn "real = %A" (spiralCount 1001)