let isPalindrome n =
    let s = List.ofSeq (sprintf "%i" n)
    s = List.rev s

let isBinaryPalindrome (n:int) = 
    let s = System.Convert.ToString(n,2) |> List.ofSeq
    s = List.rev s

let palindromicBase2And10 n = isPalindrome n && isBinaryPalindrome n

printfn "test palindromicBase2And10 585 (true) = %A" (palindromicBase2And10 585)
printfn "real = %A" ([1..999999] |> List.filter palindromicBase2And10 |> List.sum)