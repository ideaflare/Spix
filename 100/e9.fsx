let cartesian xs ys = 
    xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))

let isPythagoreanTriplet (a,b,c) =    
    (a < b && b < c) && (a * a + b * b = c * c)

let tripletProduct sum =
    cartesian [1..sum] [1..sum]
    |> Seq.map (fun (a,b) -> a, b, (sum - a - b))
    |> Seq.find isPythagoreanTriplet
    |> (fun (a,b,c) -> a * b * c)

printfn "test 12 (60) = %A" (tripletProduct 12)
printfn "real = %A" (tripletProduct 1000)