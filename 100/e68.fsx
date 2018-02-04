#load "../lib/combinatorics.fsx"
#load "../lib/numeral.fsx"

let isTriGon [a;b;c;d;e;f] =
    let rowSum = a + b + c
    a < d && a < f &&
    d + c + e = rowSum && f + e + b = rowSum

let isPentGon [a;b;c;d;e;f;g;h;i;j] =
    let rowSum = a + b + c
    a < d && a < f && a < h && a < j &&
    d + c + e = rowSum && f + e + g = rowSum &&
    h + g + i = rowSum && j + i + b = rowSum

let triGons =
    Combinatorics.permutations [1..6]
    |> Seq.filter isTriGon
    |> Seq.map (fun [a;b;c;d;e;f] -> [a;b;c;d;c;e;f;e;b])
    |> List.ofSeq

let pentGons =
    Combinatorics.permutations [1..10]
    |> Seq.filter isPentGon
    |> Seq.map (fun [a;b;c;d;e;f;g;h;i;j] -> [a;b;c; d;c;e; f;e;g; h;g;i; j;i;b])
    |> List.ofSeq

let largestGonRing rings =
    rings
    |> List.map Numeral.digitListToBigInt
    |> List.filter (fun i -> i < System.Numerics.BigInteger.Pow(10I,16))
    |> List.max

printfn "test largestGonRing triGons (432621513) = %A" (largestGonRing triGons)
printfn "real = %A" (largestGonRing pentGons)