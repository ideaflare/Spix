type Big = System.Numerics.BigInteger

let digitSum n = string n |> Seq.sumBy (string >> System.Int64.Parse)

let powDigitSum (a:int) b = Big.Pow(Big(a),b) |> digitSum

printfn "test powDigitSum 10^100 (1) = %i" (powDigitSum 10 100)

let maxDigitSum =
    seq {
        for a in 1..99 do
        for b in 1..99 do
        yield powDigitSum a b
    }
    |> Seq.max

printfn "real = %i" maxDigitSum