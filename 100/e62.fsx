let digits (b:System.Numerics.BigInteger) = b |> string |> Seq.map (string >> System.Int32.Parse)

let sortedDigits = digits >> Seq.sort >> List.ofSeq

let smallestCube nPermutations =
    let rec sc i (lookup : Map<int list,System.Numerics.BigInteger list>) =
        let cube = i * i * i
        let key = sortedDigits cube
        let members = cube :: (defaultArg (lookup.TryFind(key)) [])
        if members.Length = nPermutations
        then List.min members
        else sc (i + 1I) (lookup.Remove(key).Add(key,members))
    sc 1I Map.empty

printfn "test smallestCube 3 permutations (41063625) = %A" (smallestCube 3)
printfn "real = %A" (smallestCube 5)