
let maybeIntegralHypotenuse a b =
    let hypotenuseSquared = a * a + b * b
    let hypotenuse = sqrt (float hypotenuseSquared) |> int
    if hypotenuse * hypotenuse = hypotenuseSquared
    then Some(hypotenuse) else None

let maybeIntegralTriangle (a, b) =
    maybeIntegralHypotenuse a b
    |> Option.map (fun h -> (a,b,h))

let integralTriangles =
    seq {
        for a in 1..1000 do
        for b in a..1000 do 
        yield  (a, b)
    }
    |> Seq.choose maybeIntegralTriangle

let mostSolutionsPerimeter =
    integralTriangles
    |> Seq.groupBy (fun (a,b,c) -> a + b + c)
    |> Seq.filter (fun (perimeter,_) -> perimeter < 1000)
    |> Seq.maxBy (fun (_, solutions) -> Seq.length solutions)
    |> (fun (perimeter, _) -> perimeter)

printfn "real = %A" (mostSolutionsPerimeter)