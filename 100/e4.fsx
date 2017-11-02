let isPalindrome n =
    let s = List.ofSeq (sprintf "%i" n)
    s = List.rev s
    
let digitNumbers digits =
    let lower = System.Math.Pow(10.0, float (digits - 1)) |> int
    let upper = lower * 10 - 1
    [lower .. upper]

let largestPalindromeProduct digits =
    let ns = digitNumbers digits
    ns
    |> List.collect (fun a -> ns |> List.map (fun b ->  a * b))
    |> List.filter isPalindrome
    |> List.max

printfn "test 2 (9009) = %A" (largestPalindromeProduct 2)
printfn "real = %A" (largestPalindromeProduct 3)