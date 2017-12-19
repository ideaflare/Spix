#load "../lib/prime.fsx"

let eulersQuadratic n = n * n + n + 41L

let consecutivePrimes formula =
    Seq.initInfinite (int64 >> formula)
    |> Seq.takeWhile Prime.isPrime
    |> Seq.length

printfn "test consecutivePrimes eulersQuadratic (40) = %A" (consecutivePrimes eulersQuadratic)
printfn "test eulersQuadratic isPrime 40 (false) = %A" (eulersQuadratic 40L |> int64 |> Prime.isPrime)

let incredibleFormula n = n * n + (-79L * n) + 1601L

printfn "test consecutivePrimes incredibleFormula (80) = %A" (consecutivePrimes incredibleFormula)

let formula a b n : int64 = n * n + a * n + b

let formulaConsecutivePrimes a b = consecutivePrimes (formula a b)

let ``formula 1 41`` = formulaConsecutivePrimes 1L 41L
let ``formula -79 1601`` = formulaConsecutivePrimes -79L 1601L

printfn "test formula_1_41 = consecutivePrimes eulersQuadratic (true) = %A" (consecutivePrimes eulersQuadratic = ``formula 1 41``)
printfn "test formula_-79_1601 = consecutivePrimes eulersQuadratic (true) = %A" (consecutivePrimes incredibleFormula = ``formula -79 1601``)

let coefficients =
    [-999L .. 999L]
    |> List.collect (fun a -> 
        [-1000L .. 1000L] |> List.map (fun b -> (a,b))    
    )

let maxCoefficientProduct =
    coefficients
    |> List.maxBy (fun (a,b) -> formulaConsecutivePrimes a b)
    |> (fun (a,b) -> a * b)

printfn "real = %A" maxCoefficientProduct