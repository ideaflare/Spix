// https://en.wikipedia.org/wiki/Pell%27s_equation#Fundamental_solution_via_continued_fractions
#load "../lib/rational.fsx"
open Rational

let bigSquare = fun b -> Big.Pow(b,2)
let lhs d x y = (bigSquare x) - (d * (bigSquare y))

let sqrtFractionalExpansion n =
    let squareFloor = n |> float |> sqrt |> int |> Big
    let m, d, a = 0I, 1I, squareFloor
    if (a * a = n)
    then Seq.singleton (a /. 1I)
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

                yield (numerator /. denominator)

                yield! fractionExpansion m d a numerator denominator num den
        }
        fractionExpansion m d a num den 1I 0I
        

let findX d =
    sqrtFractionalExpansion d
    |> Seq.find (fun r -> bigSquare(r.Numerator) - d * bigSquare(r.Denominator) = 1I)
    |> (fun r -> d, r.Numerator)

let isSquare n =
    let intRoot = int (sqrt (float n))
    n = intRoot * intRoot

let largestMinimalX dMax =
    [2..dMax]
    |> List.filter (isSquare >> not)
    |> List.map (Big >> findX)
    |> List.maxBy (fun (d,x) -> x)
    |> (fun (d,_) -> d)

printfn "real = %A" (largestMinimalX 1000)
