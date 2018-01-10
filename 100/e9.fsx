let isPythagoreanTriplet (a,b,c) =    
    (a < b && b < c) && (a * a + b * b = c * c)

let tripletProduct sum =
    [1..sum]
    |> List.collect (fun a -> [a..sum] |> List.map (fun b -> a,b, sum - a - b))
    |> Seq.find isPythagoreanTriplet
    |> (fun (a,b,c) -> a * b * c)

printfn "test 12 (60) = %A" (tripletProduct 12)
printfn "real = %A" (tripletProduct 1000)