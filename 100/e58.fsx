#load "../lib/prime.fsx"

let diagonalLength ratioBelow =
    let rec e' lastEdge skip primesTotal total =
        let edge1 = lastEdge + skip
        let edge2 = edge1 + skip
        let edge3 = edge2 + skip
        let edge4 = edge3 + skip
        
        let primeEdges = (Seq.filter Prime.isPrime [edge1;edge2;edge3] |> Seq.length |> float)
        let newPrimes = primesTotal + primeEdges
        let newTotal = total + 4.0

        if (newPrimes / newTotal) < ratioBelow then (skip + 1L)
        else e' edge4 (skip + 2L) newPrimes newTotal
    e' 1L 2L 0.0 1.0

printfn "real = %i" (diagonalLength 0.1)