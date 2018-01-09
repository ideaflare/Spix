type Big = System.Numerics.BigInteger

let selfPower (n : int) = Big.Pow(Big(n),n)

let sumSelfPowersUpTo n = {1..n} |> Seq.sumBy selfPower

printfn "test selfPow^10 (10405071317) = %A" (sumSelfPowersUpTo 10)

let lastTenDigitsSelfPow1k = sumSelfPowersUpTo 1000 % Big(1e10)
    
printfn "real = %A" (lastTenDigitsSelfPow1k)

// And without BigInteger:

let sumOfPowerUpTo lastDigits n =
    let selfPowerMod n = 
        {1L..(n - 1L)} 
        |> Seq.fold (fun a _ -> (a * n) % lastDigits) n
    {1L..n}
    |> Seq.map selfPowerMod 
    |> Seq.reduce (fun a b -> (a + b) % lastDigits)

printfn "test selfPow^10 (10405071317) = %A" (sumOfPowerUpTo (int64 1e12) 10L)
printfn "real = %A" (sumOfPowerUpTo (int64 1e10) 1000L)