#load "../lib/scriptData.fsx"
let data = ScriptData.dataText __SOURCE_FILE__ __SOURCE_DIRECTORY__
let words = data.Replace("\"","").Split(',')

let triangleNumber n = n * (n + 1) / 2

let triangleSequence =
    Seq.initInfinite (fun i -> triangleNumber (i + 1))
    |> Seq.cache

printfn "test first 10 triangle numbers correct (%A)" ([1; 3; 6; 10; 15; 21; 28; 36; 45; 55] = List.map triangleNumber [1..10])

let letterValueMap =
    ['A'..'Z']
    |> List.mapi (fun idx letter -> (letter, idx + 1))
    |> Map.ofList

let wordValue word = Seq.sumBy (fun letter -> letterValueMap.[letter]) word

printfn "test word value SKY 55 (%A)" (wordValue "SKY" = 55)

let isTriangleNumber n = 
    triangleSequence
    |> Seq.skipWhile (fun t -> t < n)
    |> Seq.head = n

let isTriangleWord word = wordValue word |> isTriangleNumber

printfn "test is Triangle number SKY 55 (%A)" (isTriangleWord "SKY")

let triangleWords =
    words
    |> Seq.filter isTriangleWord
    |> Seq.length

printfn "real = %A" triangleWords