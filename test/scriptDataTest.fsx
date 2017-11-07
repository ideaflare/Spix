#load "../lib/scriptData.fsx"

let ``dataText`` () =
    let actual = ScriptData.dataText __SOURCE_FILE__ __SOURCE_DIRECTORY__
    let expected = "test 123456"
    printfn "dataText : expected %A actual %A -> %A" expected actual (expected = actual)

let ``dataRows`` () =
    let actual = ScriptData.dataRows __SOURCE_FILE__ __SOURCE_DIRECTORY__
    let expected = ["test 123";"456"]
    printfn "dataRows : expected %A actual %A -> %A" expected actual (expected = actual)

``dataText`` () 
``dataRows`` () 