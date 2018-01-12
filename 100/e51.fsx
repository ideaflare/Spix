#load "../lib/numeral.fsx"
#load "../lib/prime.fsx"

let rec trimLeadingZeroes = function
    | x :: xs when x = 0 -> trimLeadingZeroes xs
    | xs -> xs

let createPrimeFamily digits replaceTarget =
    let replaceDigit replacement = 
        digits 
        |> List.map (fun digit -> 
            if digit = replaceTarget
            then replacement else digit)
        |> trimLeadingZeroes
    [0..9]
    |> List.map (replaceDigit)
    |> List.filter (fun familyMember -> familyMember.Length = digits.Length)
    |> List.filter (Numeral.digitListToInt >> int64 >> Prime.isPrime)

let largestPrimeFamily n =
    let digits = Numeral.digits n
    digits
    |> List.distinct
    |> List.map (createPrimeFamily digits)
    |> List.maxBy List.length

let firstPrimeInFamily minFamilySize =
    Prime.sequence
    |> Seq.map int
    |> Seq.find (fun i -> List.length (largestPrimeFamily i) >= minFamilySize)
    |> largestPrimeFamily
    |> Seq.head
    |> Numeral.digitListToInt

printfn "test firstPrimeInFamily with size 6 (13) = %A" (firstPrimeInFamily 6)
printfn "test firstPrimeInFamily with size 7 (56003) = %A" (firstPrimeInFamily 7)

printfn "real = %A" (firstPrimeInFamily 8)