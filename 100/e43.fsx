#load "../lib/numeral.fsx"
#load "../lib/combinatorics.fsx"

let div2 n = n % 2 = 0
let div3 x y z = (x + y + z) % 3 = 0
let div5 n = n = 0 || n = 5

let digitsDiviby n digits = (Numeral.digitListToInt digits) % n = 0
let div7 digits = digitsDiviby 7 digits
let div11 digits = digitsDiviby 11 digits
let div13 digits = digitsDiviby 13 digits
let div17 digits = digitsDiviby 17 digits

let divisibleProperty = function
    | [_;_;d3;d4;d5;d6;d7;d8;d9;d10] ->
        div2 d4 && div3 d3 d4 d5 && div5 d6
        && div7  [d5;d6;d7]
        && div11 [d6;d7;d8]
        && div13 [d7;d8;d9]
        && div17 [d8;d9;d10]
    | _ -> false

printfn "test divisibleProperty 1406357289 (%A)" (divisibleProperty (Numeral.digits 1406357289))

let divisiblePandigitalSum = 
    Combinatorics.permutations [0..9]
    |> Seq.filter divisibleProperty
    |> Seq.sumBy Numeral.digitListToBigInt

printfn "real = %A" divisiblePandigitalSum