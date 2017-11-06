#load "../lib/scriptData.fsx"

let ``dataText`` () =
    let actual = ScriptData.dataText __SOURCE_FILE__ __SOURCE_DIRECTORY__
    let expected = "test 123"
    printfn "dataText : expected %A actual %A -> %A" expected actual (expected = actual)

``dataText`` () 