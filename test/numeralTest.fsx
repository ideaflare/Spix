#load "../lib/numeral.fsx"

let ``Zero has one digit zero`` () =
    let actual = Numeral.digits 0
    let expected = [0]
    printfn "Digits of 0 : expected %A actual %A -> %A" expected actual (expected = actual)

let ``Collect digits of number`` () =
    let actual = Numeral.digits 4709503
    let expected = "4709503" |> Seq.map (string >> System.Int32.Parse) |> List.ofSeq
    printfn "Digits of 543210 : expected %A actual %A -> %A" expected actual (expected = actual)

``Zero has one digit zero`` ()
``Collect digits of number`` ()