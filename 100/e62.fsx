let digits = string >> Seq.map (string >> System.Int32.Parse)

let sortedDigits (b: System.Numerics.BigInteger) =  b |> digits |> Seq.sort |> List.ofSeq

let cube (n:System.Numerics.BigInteger) = n * n * n

let smallestCube nPermutations =
    let rec sc i (lookup : Map<int list,System.Numerics.BigInteger list>) =
        let iCube = cube i
        let key = sortedDigits iCube
        match lookup.TryFind(key) with
        | Some(members) ->
            let newMembers = iCube :: members
            if List.length newMembers = nPermutations
            then Seq.min members
            else
                let newLookup = lookup.Remove(key).Add(key,newMembers)
                sc (i + 1I) newLookup
        | None ->
            sc (i + 1I) (lookup.Add(key,[iCube]))
    sc 1I Map.empty

printfn "test smallestCube 3 permutations (41063625) = %A" (smallestCube 3)
printfn "real = %A" (smallestCube 5)