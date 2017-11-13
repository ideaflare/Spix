#load "../lib/prime.fsx"
open Prime

let triangleNumber n = (n + 1) * n / 2

let testFirst10TriangleNumbers = [1..10] |> List.map triangleNumber
let first10TriangleNumbers = [1; 3; 6; 10; 15; 21; 28; 36; 45; 55]

printfn "test triangleNumber matches first 10 expected numbers = %A" (testFirst10TriangleNumbers = first10TriangleNumbers)
printfn "Number of divisors is hypercube of prime factors exponents, including pow(prime,0)"

let divisors (n : int) =
    let factors = Prime.factors (int64 n) |> List.map int
    let exponentsTotals =
        factors 
        |> List.countBy id 
        |> List.map (fun (_ ,count) -> count + 1)
    exponentsTotals |> List.fold (*) 1

printfn "test divisors 28 (6) = %A" (divisors 28)

let divisorsHalfWhenEven n =
    let halfWhenEven =
        match n % 2 = 0 with
        | true -> n / 2
        | false -> n
    divisors halfWhenEven

let triangleDivisors n =
    (divisorsHalfWhenEven n) * (divisorsHalfWhenEven (n + 1))    

let triangleOverNDivisors x = 
    seq { 1 .. System.Int32.MaxValue }
    |> Seq.tryFind (fun n -> triangleDivisors n > x)
    |> Option.map triangleNumber

#time "on"
printfn "test 5 (28) = %A" (triangleOverNDivisors 5)
printfn "real = %A" (triangleOverNDivisors 500)