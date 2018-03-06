#load "../lib/rational.fsx"
open Rational

let eApproximationSeries nTh =
    let two  = 2I /. 1I
    let one = 1I /. 1I
    let k2Indexes = [2..3..(5 + nTh)]
    let kVal n =
        match List.tryFindIndex ((=) n) k2Indexes with
        | (Some k) -> ((Big (k + 1)) * 2I) /. 1I
        | None -> one
    let eFractionSeries = List.map kVal [1..nTh]
    let reversedSeries = eFractionSeries |> List.rev
    let foldUpwards lowNum leftNum = add leftNum (inverse lowNum)
    let expansion = List.reduce foldUpwards reversedSeries
    add two (inverse expansion)

let hundrethExpansion = eApproximationSeries 99

let bigDigits (b:Big) = (Seq.map (string >> Big.Parse) (b |> string))

let numeralSum = hundrethExpansion.Numerator |> bigDigits |> Seq.sum

printfn "real = %A" numeralSum