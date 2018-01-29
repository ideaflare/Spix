type Big = System.Numerics.BigInteger

#load "../lib/function.fsx"

let bigSquare = fun b -> Big.Pow(b,2)

let rootPow b = (bigSquare b, b)

let bigSqrt (b:Big) =
    let possibleRoot = Seq.initInfinite Big |> Seq.skip 1 |> Seq.find (fun i -> i * i >= b)
    if possibleRoot * possibleRoot = b then Some(possibleRoot) else None

let hasYSolution d x =
    let lhs x y = (bigSquare x) - (d * (bigSquare y))
    let xSquareMin1 = (bigSquare x) - 1I
    let ySquared = xSquareMin1 / d
    if xSquareMin1 = d * ySquared
    then
        match bigSqrt ySquared with
        | Some(y)  -> 
            printfn "y %A" y
            true
        | _ -> false
    else false

let findX d =
    let rec fx' d x =
        if hasYSolution d x 
        then
            printfn "D[%A] x %A" d x
            x
        else fx' d (x + 1I)
    fx' d 1I

let isSquare n =
    let intRoot = int (sqrt (float n))
    n = intRoot * intRoot

let largestMinimalX dMax =
    [2..dMax]
    |> List.filter (isSquare >> not)
    |> List.map (Big >> findX)
    |> List.max

#time
printfn "real = %A" (largestMinimalX 65)