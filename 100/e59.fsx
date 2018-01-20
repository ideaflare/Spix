#load "../lib/scriptData.fsx"
let data = ScriptData.dataCommaSeparatedLine __SOURCE_FILE__ __SOURCE_DIRECTORY__ |> List.ofArray |> List.map int

let groups = data |> List.ofSeq |> List.chunkBySize 3

let keys =
    seq {
        for k in 'a'..'z' do
        for e in 'a'..'z' do
        for y in 'a'..'z' do
        yield (int k,int e,int y)
    }

let the = "the" |> Seq.map int |> List.ofSeq

let xorChunk k e y = function
    | [a;b;c] -> [k ^^^ a;e ^^^ b;y ^^^ c]
    | [a;b] -> [k ^^^ a;e ^^^ b]
    | [a] -> [k ^^^ a]
    | xs -> xs

let decrypt (k,e,y) =
    groups
    |> List.map (xorChunk k e y)
    |> List.collect id

let theCount (k,e,y) =
    decrypt (k,e,y)
    |> List.windowed 3
    |> List.filter (fun chunk -> chunk = the)
    |> Seq.length

let key = keys |> Seq.maxBy theCount

printfn "real = %A" (decrypt key |> Seq.sum)
