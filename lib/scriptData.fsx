let dataPath (sourceFile: string) (sourceDirectory: string) =
    let dataFile = sourceFile.Replace(".fsx",".data")
    let dataPath = System.IO.Path.Combine(sourceDirectory,"..","data/" + dataFile)
    dataPath

let dataText sourceFile sourceDirectory =
    dataPath sourceFile sourceDirectory
    |> System.IO.File.ReadAllLines
    |> (fun lines -> System.String.Join("", lines))
    