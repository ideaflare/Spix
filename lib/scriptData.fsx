let dataPath (sourceFile: string) (sourceDirectory: string) =
    let dataFile = sourceFile.Replace(".fsx",".data")
    let dataPath = System.IO.Path.Combine(sourceDirectory,"..","data/" + dataFile)
    dataPath

let dataRows sourceFile sourceDirectory =
    dataPath sourceFile sourceDirectory
    |> System.IO.File.ReadAllLines
    |> List.ofArray

let dataText sourceFile sourceDirectory =
    dataRows sourceFile sourceDirectory
    |> (fun lines -> System.String.Join("", lines))

let splitToCells (txt : string) =
    txt.Split [|' '|]
    |> List.ofArray
    |> List.map int

let parseGrid = List.map splitToCells