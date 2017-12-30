#load "../lib/numeral.fsx"
let digits = Numeral.digits

let isPandigital digits = List.sort digits = [1..9]

let concatenatedProduct i =
    let multiplyDigits = ((*) i) >> digits
    let rec times' n (acc : int list) =
        match acc with
        | reached when n > 2 && acc.Length >= 9 -> reached
        | r ->
            let nDigits = multiplyDigits n
            times' (n + 1) (acc @ nDigits)
    (times' 1 [])

let digitListToInt digits =
    let rec d' n sum = function
        | [] -> sum
        | x :: xs -> d' (n * 10) (sum + x * n) xs
    d' 1 0 (List.rev digits)

printfn "test concatenatedProduct 192 (192384576) = %A" (concatenatedProduct 192)
printfn "test concatenatedProduct 9 (918273645) = %A" (concatenatedProduct 9)
printfn "test isPandigital concatProduct 192 (true) = %A" (isPandigital (concatenatedProduct 192))
printfn "test digitListToInt [6;3;0;7;0] (63070) = %A" (digitListToInt [6;3;0;7;0])

let maxConcatsPandigitalsUnder n =
    [2..n]
    |> List.map concatenatedProduct
    |> List.filter isPandigital
    |> List.map digitListToInt
    |> List.max

printfn "real = %A" (maxConcatsPandigitalsUnder 10000)

