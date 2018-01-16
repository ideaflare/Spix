type Big = System.Numerics.BigInteger

let isPalindrome n =
    let s = List.ofSeq (string n)
    s = List.rev s

let isLychrel n =
    let rec lyc' i num =
        if i = 50 then true
        else
            let reverseStr = (string num) |> Seq.rev |> Array.ofSeq |> (fun s -> System.String(s))
            let reverseNum = Big.Parse(reverseStr)
            let sum = reverseNum + num
            if isPalindrome sum then false
            else lyc' (i + 1) sum
    lyc' 1 n

printfn "test isLychrel 47 (false) = %b" (isLychrel 47I)
printfn "test isLychrel 349 (false) = %b" (isLychrel 349I)
printfn "test isLychrel 196 (true) = %b" (isLychrel 196I)
printfn "test isLychrel 10677 (*true) = %b *under constraints of this problem" (isLychrel 10677I)
printfn "test isLychrel 4994 (true) = %b" (isLychrel 4994I)

printfn "real = %A" ([1..9999] |> Seq.map Big |> Seq.filter isLychrel |> Seq.length)