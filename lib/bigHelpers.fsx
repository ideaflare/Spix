let rec incrementTowardsSquare guess square =
    let squareGuess = guess * guess
    if squareGuess = guess then Some(guess)
    else 
        if squareGuess < square
        then
            let g = {guess..squareGuess} |> Seq.map (fun g -> g, g * g) |> Seq.skipWhile (fun (g, gSq) -> gSq < square) |> Seq.head
            let gh = fst g
            if gh * gh = square then Some gh else None
        else
            let g = {guess .. -1I .. 0I} |> Seq.map (fun g -> g, g * g) |> Seq.skipWhile (fun (g, gSq) -> gSq > square) |> Seq.head
            let gh = fst g
            if gh * gh = square then Some gh else None

let newtonRaphson fx dx target =
    let rec nr' guess (previousGuesses:Set<System.Numerics.BigInteger>) i =
        let newGuess = guess - ((fx guess) / (dx guess))
        if previousGuesses.Contains(newGuess) || i > 100
        then incrementTowardsSquare guess target
        else nr' newGuess (previousGuesses.Add(newGuess)) (i + 1)
    nr' 1I Set.empty 1

let dx x = 2I * x
let fx (target:System.Numerics.BigInteger) x = x * x - target

let maybeSquare x = 
    let fxG = fx x
    newtonRaphson fxG dx x