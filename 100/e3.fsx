let largestFactor num =
    let rec cut prime num =
        match num % prime = 0L with
        | true -> cut prime (num / prime)
        | false -> 
            if num <= prime then prime
            else cut (prime + 1L) num
    cut 2L num

printfn "test 13195 (29) = %A" (largestFactor 13195L)
printfn "real = %A" (largestFactor 600851475143L)
