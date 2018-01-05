let triangle n = n * (n + 1I) / 2I
let pentagonal n = n * (3I * n - 1I) / 2I
let hexagonal n = n * (2I * n - 1I)

let isMember f maxMembers =
    let fMembers = List.map f [1I..maxMembers]
    let lookup = Set.ofList fMembers
    fun n -> lookup.Contains(n)

let isPentagonal = isMember pentagonal 100000I
let isHexagonal = isMember hexagonal 100000I

let triPentHexNum above =
    {0I .. 999999I } 
    |> Seq.map triangle
    |> Seq.filter (fun n -> n > above)
    |> Seq.filter isPentagonal
    |> Seq.filter isHexagonal
    |> Seq.head

printfn "test triPentHexNum above 1 (40755) = %A" (triPentHexNum 1I)
printfn "real = %A" (triPentHexNum 40755I)