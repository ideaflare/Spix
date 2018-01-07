#load "../lib/prime.fsx"

let twiceSquares =
    Seq.initInfinite int64
    |> Seq.skip 1
    |> Seq.map (fun n -> 2L * n * n)
    |> Seq.cache

let primeHasSquaresum p n =
    let requiredSquare = n - p
    twiceSquares
    |> Seq.takeWhile (fun sq -> sq <= requiredSquare)
    |> Seq.exists ((=) requiredSquare)

let isGoldbachComposite n =
    Prime.sequence
    |> Seq.takeWhile (fun p -> p < n)
    |> Seq.exists (fun p -> primeHasSquaresum p n)

let notGoldbachOddComposite =
    {3L .. 2L .. System.Int64.MaxValue }
    |> Seq.filter (Prime.isPrime >> not)
    |> Seq.filter (isGoldbachComposite >> not)
    |> Seq.head

printfn "real = %A" notGoldbachOddComposite