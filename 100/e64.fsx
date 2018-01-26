// https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion

let sqrtFractionalExpansion n =
    let squareFloor = n |> float |> sqrt |> int
    let m, d, a = 0, 1, squareFloor
    let aSeq = Seq.singleton a
    if (a * a = n)
    then aSeq
    else
        let endSearch = 2 * squareFloor
        let rec fractionExpansion m d a =
            seq {
                let m = d * a - m;                    
                let d = (n - m * m) / d;
                let a = (squareFloor + m) / d;
                yield a
                if a <> endSearch then yield! fractionExpansion m d a
        }
        Seq.append aSeq (fractionExpansion m d a)

printfn "test sqrt 23 expansion [4;(1,3,1,8)] = %A" (sqrtFractionalExpansion 23 |> Seq.toList)

let fractionalPeriod n =
    sqrtFractionalExpansion n
    |> Seq.skip 1
    |> Seq.length

printfn "test fractionalPeriod 23 (4) = %A" (fractionalPeriod 23)

let isOdd n = n % 2 <> 0
let oddSqrtExpantionsUpTo n = [2..n] |> List.filter (fractionalPeriod >> isOdd) |> List.length

printfn "test oddSqrtExpantionsUpTo 13 (4) = %A" (oddSqrtExpantionsUpTo 13)

printfn "real = %A" (oddSqrtExpantionsUpTo 10000)