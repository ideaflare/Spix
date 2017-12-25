#load "../lib/numeral.fsx"

let lowestCommonTerms (n,d) =
    let rec lcd a b test =
        match test with
        | [] -> (a,b)
        | h :: t ->
            if a % h = 0 && b % h = 0 then lcd (a / h) (b / h) test
            else lcd a b t
    lcd n d [2..(max n d)]

printfn "test lowestCommonTerms 49/98 (1/2) = %A" (lowestCommonTerms (49,98))
printfn "test lowestCommonTerms 432,7934 (216/3967) = %A" (lowestCommonTerms (432,7934))

let curiousFraction (n,d) =
    let simplified = lowestCommonTerms (n,d)
    if simplified = (n,d) then false
    else
        let nDigits = Numeral.digits n
        let dDigits = Numeral.digits d        
        let maybeCancelFraction n d =
            if nDigits.[n] = dDigits.[d] then Some(nDigits.[1 - n],dDigits.[1 - d])
            else None
        let cancelFractions =
            [0;1]
            |> List.collect (fun n -> [0;1] |> List.map (fun d -> maybeCancelFraction n d))
            |> List.choose id 
            |> List.map lowestCommonTerms
        cancelFractions |> List.exists ((=) simplified)

printfn "test curiousFraction 49/98 (true) = %A" (curiousFraction (49,98))
printfn "test curiousFraction 1/2 (false) = %A" (curiousFraction (1,2))

let search =
    [11..99]
    |> List.collect (fun numerator -> [1..99] |> List.map (fun denominator -> (numerator,denominator)))
    |> List.filter (fun (n,d) -> n < d)
    |> List.filter (fun (n,d) -> not (n % 10 = 0 && d % 10 = 0))

let curiousDenominator =
    search
    |> List.filter curiousFraction
    |> List.map lowestCommonTerms
    |> List.reduce (fun (a1,b1) (a2,b2) -> (a1 * a2, b1 * b2))
    |> (fun fractionsProdcut -> lowestCommonTerms fractionsProdcut)
    |> (fun (_, denominator) -> denominator)

printfn "real = %A" curiousDenominator