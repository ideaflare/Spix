// https://en.wikipedia.org/wiki/Pell%27s_equation#Fundamental_solution_via_continued_fractions

type Big = System.Numerics.BigInteger

let bigSquare = fun b -> Big.Pow(b,2)
<<<<<<< HEAD

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
=======
let lhs d x y = (bigSquare x) - (d * (bigSquare y))

type Rational = { Numerator : Big; Denominator : Big} with
    static member Create n d = {Numerator = n; Denominator = d}
    static member Text (r:Rational) = sprintf "%A/%A" r.Numerator r.Denominator

let sqrtFractionalExpansion n =
    let squareFloor = n |> float |> sqrt |> int |> Big
    let m, d, a = 0I, 1I, squareFloor
    if (a * a = n)
    then Seq.singleton (Rational.Create a 1I)
    else
        let num = squareFloor
        let den = 1I
        let rec fractionExpansion m d a num den numPrev denPrev =
            seq {
                let m = d * a - m;                    
                let d = (n - m * m) / d;
                let a = (squareFloor + m) / d;

                let numerator = a * num + numPrev
                let denominator = a * den + denPrev

                yield (Rational.Create numerator denominator)

                yield! fractionExpansion m d a numerator denominator num den
        }
        fractionExpansion m d a num den 1I 0I
        

let findX d =
    sqrtFractionalExpansion d
    |> Seq.find (fun r -> bigSquare(r.Numerator) - d * bigSquare(r.Denominator) = 1I)
    |> (fun r -> d, r.Numerator)
>>>>>>> 66

let isSquare n =
    let intRoot = int (sqrt (float n))
    n = intRoot * intRoot

let largestMinimalX dMax =
    [2..dMax]
    |> List.filter (isSquare >> not)
    |> List.map (Big >> findX)
<<<<<<< HEAD
    |> List.maxBy (fun (x,d) -> x)
    |> (fun (x,d) -> d)

#time
printfn "real = %A" (largestMinimalX 7)
=======
    |> List.maxBy (fun (d,x) -> x)
    |> (fun (d,_) -> d)

#time
printfn "real = %A" (largestMinimalX 1000)
>>>>>>> 66
