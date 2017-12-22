let powMap n =
    [0..9]
    |> List.map (fun i -> (i, System.Math.Pow(float i, float n) |> int ))
    |> Map.ofList

let digits n =
    n.ToString()
    |> Seq.map (string >> System.Int32.Parse)

let sameAsDigitPowSum (powMap : Map<int,int>) n = 
    let digitSum = digits n |> Seq.sumBy (fun i -> powMap.[i])
    digitSum = n


printfn "test 4 power digit sums ([1634; 8208; 9474]) = %A" ([2 .. 9999] |> List.filter (sameAsDigitPowSum (powMap 4)))

printfn "real = %A" ([2 .. 999999] |> List.filter (sameAsDigitPowSum (powMap 5)) |> List.sum)