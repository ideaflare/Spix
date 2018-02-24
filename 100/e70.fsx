#load "../lib/prime.fsx"

type RationalInt64 = { Numerator : int64; Denominator : int64} with
    static member FromNumeratorDenominator n d = {Numerator = n; Denominator = d}
    static member FromNumerator n = {Numerator = n; Denominator = 1L}

let add r1 r2 = { Numerator = r1.Numerator * r2.Denominator + r2.Numerator * r1.Denominator;
                  Denominator = r1.Denominator * r2.Denominator }

let multiply r1 r2 = { Numerator = r1.Numerator * r2.Numerator;
                       Denominator = r1.Denominator * r2.Denominator }

let phi n =
    Prime.factors n
    |> Seq.distinct
    |> Seq.map (fun p -> RationalInt64.FromNumeratorDenominator (p - 1L) p)
    |> Seq.fold multiply (RationalInt64.FromNumerator n)
    |> (fun r -> r.Numerator / r.Denominator)