let triangle n = n * (n+1) / 2
let square n = n * n
let pentagonal n = n * (3 * n - 1) / 2
let hexagonal n = n * (2 * n - 1)
let heptagonal n = n * (5 * n - 3 ) / 2
let octagonal n = n * (3 * n - 2)

#load "../lib/numeral.fsx"
#load "../lib/function.fsx"

let digits = Function.memoize Numeral.digits

let polygonalSequence f =
    Seq.initInfinite f
    |> Seq.skipWhile ((>) 1000)
    |> Seq.takeWhile ((>) 10000)
    |> List.ofSeq

let polygonals = [octagonal;heptagonal;hexagonal;pentagonal;square;triangle] |> List.map polygonalSequence

let isValidSuccession a b =
    match digits a, digits b with
    | [_;_;a3;a4], [b1;b2;_;_] when a3 = b1 && a4 = b2 -> true
    | _ -> false

let successions aSequence bSequence =
    let a = List.last aSequence
    bSequence
    |> List.filter (fun b -> isValidSuccession a b)
    |> List.map (fun succession -> aSequence @ [succession])

let orderedSet polygonalOrders =
    match polygonalOrders with
    | [a;b;c;d;e;f] ->
        polygonals.[a]
        |> List.collect (fun o -> successions [o] polygonals.[b])
        |> List.collect (fun o -> successions o polygonals.[c])
        |> List.collect (fun o -> successions o polygonals.[d])
        |> List.collect (fun o -> successions o polygonals.[e])
        |> List.collect (fun o -> successions o polygonals.[f])
        |> List.filter (fun r -> isValidSuccession (List.last r) (List.head r))
        |> List.tryHead
    | _ -> None

#load "../lib/combinatorics.fsx"

let orders =
    Combinatorics.exhaustivePermutations [0..5]
    |> Seq.choose orderedSet
    |> Seq.head

printfn "real = %A" (orders |> Seq.sum)