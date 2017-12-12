let properDivisors n =
    [1.. (n / 2)]
    |> List.filter (fun i -> n % i = 0)

let sumProperDivisors = properDivisors >> List.sum

let amicablePairs searchRange =
     let divisorSumLookup = 
        searchRange
        |> List.map (fun lookup -> (lookup, sumProperDivisors lookup))
        |> Map.ofList
     let isAmicableNumber n =
        match divisorSumLookup.TryFind(n) with
        | Some(pair) when pair <> n && divisorSumLookup.TryFind(pair) = Some(n) -> true
        | _ -> false
     searchRange
     |> List.filter isAmicableNumber

let sumAmicablePairs = amicablePairs >> List.sum

let expectedProperDivosors220 = [1; 2; 4; 5; 10; 11; 20; 22; 44; 55;110]
let actualProperDivisors220 = properDivisors 220
printfn "test properDivisors 220(%A) = %A [%A]" expectedProperDivosors220 actualProperDivisors220 (expectedProperDivosors220 = actualProperDivisors220)

printfn "test sumProperDivisors 220 (284) = %A" (sumProperDivisors 220)
printfn "test sumProperDivisors 284 (220) = %A" (sumProperDivisors 284)

printfn "test amicablePairs [200;300] (220;284) = %A" (amicablePairs [200..300])

printfn "real = %A" (sumAmicablePairs [1..10000])