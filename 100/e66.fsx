type Big = System.Numerics.BigInteger

#load "../lib/function.fsx"

let bigSquare = fun b -> Big.Pow(b,2)

let rootPow b = (bigSquare b, b)

#load "../lib/bigHelpers.fsx"
let bigSqrt = BigHelpers.maybeSquare

 // x^2 - D.y^2 = 1
 // x^2 - 1 = d.y^2
 // (x^2 - 1)/d = y^2

let hasYSolution d x =
    let lhs x y = (bigSquare x) - (d * (bigSquare y))
    let xSquareMin1 = (bigSquare x) - 1I
    let ySquared = xSquareMin1 / d
    if ySquared > 0I && xSquareMin1 = d * ySquared
    then
        // printfn "ySquared %A" ySquared
        match bigSqrt ySquared with
        | Some(y)  -> 
            // printfn "y %A" y
            true
        | _ -> false
    else false

let findX d =
    let rec fx' d x =
        if hasYSolution d x 
        then
            printfn "D[%A] x %A" d x
            (x,d)
        else fx' d (x + 1I)
    fx' d 1I

let isSquare n =
    let intRoot = int (sqrt (float n))
    n = intRoot * intRoot

let largestMinimalX dMax =
    [2..dMax]
    |> List.filter (isSquare >> not)
    |> List.map (Big >> findX)
    |> List.maxBy (fun (x,d) -> x)
    |> (fun (x,d) -> d)

#time
printfn "real = %A" (largestMinimalX 7)