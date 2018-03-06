#load "../lib/rational.fsx"
open Rational

let squareApproximationSeries =
    let one = 1I /. 1I
    let two = 2I /. 1I
    let rec series n =
        seq {
            let i = inverse (add two n)
            yield (add one i)
            yield! series i
        }
    series (0I /. 1I)

let digits = string >> Seq.length

let numeratorMoreDigits =
    squareApproximationSeries
    |> Seq.take 1000
    |> Seq.filter (fun r -> digits r.Numerator > digits r.Denominator)
    |> Seq.length

printfn "real = %A" numeratorMoreDigits