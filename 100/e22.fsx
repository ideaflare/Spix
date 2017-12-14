#load "../lib/scriptData.fsx"
let data = ScriptData.dataText __SOURCE_FILE__ __SOURCE_DIRECTORY__
let names = data.Replace("\"","").Split(',')

let alphabeticalValue : string -> int = Seq.sumBy (fun c -> (int c) - (int 'A') + 1)
let score names =
    names
    |> Seq.sort
    |> Seq.mapi (fun index name -> (index + 1) * (alphabeticalValue name))
    |> Seq.sum

printfn "test alphabeticalValue COLIN (53) = %A" (alphabeticalValue "COLIN")
printfn "real = %A" (score names)