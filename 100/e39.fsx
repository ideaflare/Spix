
let maybeIntegralHypotenuse a b =
    let hypotenuseSquared = a * a + b * b
    let hypotenuse = sqrt (float hypotenuseSquared) |> int
    if hypotenuse * hypotenuse = hypotenuseSquared
    then Some(hypotenuse) else None

type Triangle = { Sides: int * int * int } with
    member me.Perimeter = 
        let (a,b,c) = me.Sides
        a + b + c

let maybeIntegralTriangle a b =
    maybeIntegralHypotenuse a b
    |> Option.map (fun h -> {Sides = a,b,h})

let integralTriangles =
    [1..1000]
    |> List.collect (fun a -> [a..1000] |> List.map (fun b -> maybeIntegralTriangle a b))
    |> List.choose id

let mostSolutionsPerimeter =
    integralTriangles
    |> List.groupBy (fun t -> t.Perimeter)
    |> List.filter (fun (perimeter,_) -> perimeter < 1000)
    |> List.maxBy (fun (_, solutions) -> solutions.Length)
    |> (fun (perimeter, _) -> perimeter)

printfn "real = %A" (mostSolutionsPerimeter)