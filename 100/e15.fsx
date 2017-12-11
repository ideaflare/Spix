#load "../lib/combinatorics.fsx"

// Explanation
// Eacn path goes down [grid size] times, and right [grid size] times.
// This is an ordering problem https://math.stackexchange.com/a/452

let paths gridSize =
    let allOrderings = Combinatorics.factorial (gridSize * 2I)
    let downOrRight = Combinatorics.factorial gridSize
    allOrderings / (downOrRight * downOrRight)


printfn "test 0 (1) = %A" (paths 0I)
printfn "test 1 (2) = %A" (paths 1I)
printfn "test 2 (6) = %A" (paths 2I)
printfn "test 3 (20) = %A" (paths 3I)
printfn "test 4 (70) = %A" (paths 4I)
printfn "test 5 (252) = %A" (paths 5I)
printfn "real = %A" (paths 20I)

(*
1                        0  
1  2                     1
1  3  6                  2   
1  4 10  20              3   
1  5 15  35 70           4
1  6 21  56 126 252      5
*)