type Big = System.Numerics.BigInteger

let digits (b:Big) = b |> string |> Seq.map (string >> System.Int32.Parse)

let sortedDigits = digits >> Seq.sort >> List.ofSeq

let cube (n:Big) = n * n * n

let smallestCube nPermutations =
    let rec sc i (lookup : Map<int list,System.Numerics.BigInteger list>) =
        let iCube = cube i
        let key = sortedDigits iCube
        let members = iCube :: (defaultArg (lookup.TryFind(key)) [])
        if members.Length = nPermutations
        then List.min members
        else sc (i + Big.One) (lookup.Remove(key).Add(key,members))
    sc Big.One Map.empty

printfn "test smallestCube 3 permutations (41063625) = %A" (smallestCube 3)
printfn "real = %A" (smallestCube 5)