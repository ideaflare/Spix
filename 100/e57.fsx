type Big = System.Numerics.BigInteger

type Rational = { Numerator : Big; Denominator : Big} with
    static member Create n d = {Numerator = n; Denominator = d}

let add r1 r2 = { Numerator = r1.Numerator * r2.Denominator + r2.Numerator * r1.Denominator;
                  Denominator = r1.Denominator * r2.Denominator }

let inverse rational = { Numerator = rational.Denominator; Denominator = rational.Numerator }

let squareApproximationSeries =
    let one  = Rational.Create 1I 1I
    let two  = Rational.Create 2I 1I
    let rec series n =
        seq {
            let i = inverse (add two n)
            yield (add one i)
            yield! series i
        }
    series (Rational.Create 0I 1I)

let digits = string >> Seq.length

let numeratorMoreDigits =
    squareApproximationSeries
    |> Seq.take 1000
    |> Seq.filter (fun r -> digits r.Numerator > digits r.Denominator)
    |> Seq.length

printfn "real = %A" numeratorMoreDigits