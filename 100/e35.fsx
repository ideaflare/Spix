#load "../lib/prime.fsx"
#load "../lib/numeral.fsx"
   
let rotations xs =
    let length = xs |> List.length
    let perm n = xs |> List.permute (fun index -> (index + n) % length) 
    [1 .. length] |> List.rev |> List.map perm

let combineDigits digits =
    digits |> List.reduce (fun acc digit -> (acc * 10) + digit)

let combineGivesSameDigit =
    [0..123] |> List.map (Numeral.digits) |> List.map combineDigits = [0..123]

printfn "test combineGivesSameDigit (true) = %A" (combineGivesSameDigit)

let primesUnder1Mil =
    Prime.sequence
    |> Seq.takeWhile (fun p -> p < 1000000L)
    |> Set.ofSeq

let isPrime n = primesUnder1Mil.Contains n

let isRotationalPrime prime =
    Numeral.digits prime
    |> rotations
    |> List.forall (combineDigits >> int64 >> isPrime)

let rotationalPrimesBelow max =
    Prime.sequence 
    |> Seq.takeWhile (fun p -> p < max)
    |> Seq.filter (int >> isRotationalPrime)
    |> Seq.length

printfn "test 100 (13) = %A" (rotationalPrimesBelow 100L)

printfn "real = %A" (rotationalPrimesBelow 1000000L)