type Big = System.Numerics.BigInteger

type Rational = { Numerator : Big; Denominator : Big} with
    static member Create n d = {Numerator = n; Denominator = d}

let add r1 r2 = { Numerator = r1.Numerator * r2.Denominator + r2.Numerator * r1.Denominator;
                  Denominator = r1.Denominator * r2.Denominator }

let inverse rational = { Numerator = rational.Denominator; Denominator = rational.Numerator }

let eApproximationSeries nTh =
    let two  = Rational.Create 2I 1I
    let one = Rational.Create 1I 1I
    let k2Indexes = [2..3..(5 + nTh)]
    let kVal n =
        match List.tryFindIndex ((=) n) k2Indexes with
        | (Some k) -> Rational.Create ((Big (k + 1)) * 2I) 1I
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