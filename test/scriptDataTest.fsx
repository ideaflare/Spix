#load "../lib/scriptData.fsx"

let ``dataText`` () =
    let actual = ScriptData.dataText __SOURCE_FILE__ __SOURCE_DIRECTORY__
    let expected = "test 123456"
    printfn "dataText : expected %A actual %A -> %A" expected actual (expected = actual)

let ``dataRows`` () =
    let actual = ScriptData.dataRows __SOURCE_FILE__ __SOURCE_DIRECTORY__
    let expected = ["test 123";"456"]
    printfn "dataRows : expected %A actual %A -> %A" expected actual (expected = actual)

let ``parseGrid`` () =
    let gridText = "01 02 03
04 05 06
08 09 10 11 12"
    let gridRows = gridText.Split('\n') |> List.ofSeq
    let actual = ScriptData.parseGrid gridRows
    let expected = [[1; 2; 3]; [4; 5; 6]; [8; 9; 10; 11; 12]]
    printfn "dataRows : expected %A actual %A -> %A" expected actual (expected = actual)

``dataText`` () 
``dataRows`` ()
``parseGrid`` ()