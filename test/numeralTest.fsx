#load "../lib/numeral.fsx"

let ``Zero has one digit zero`` () =
    let actual = Numeral.digits 0
    let expected = [0]
    printfn "Digits of 0 : expected %A actual %A -> %A" expected actual (expected = actual)

let ``Collect digits of number`` () =
    let actual = Numeral.digits 4709503
    let expected = "4709503" |> Seq.map (string >> System.Int32.Parse) |> List.ofSeq
    printfn "Digits of 543210 : expected %A actual %A -> %A" expected actual (expected = actual)

let ``No digits converts to 0`` () =
    let actual = Numeral.digitListToInt []
    let expected = 0
    printfn "Numeral of digits [] : expected %A actual %A -> %A" expected actual (expected = actual)

let ``Digits [0;1;5;0;2] converts to 1502`` () =
    let actual = Numeral.digitListToInt [0;1;5;0;2]
    let expected = 1502
    printfn "Numeral of digits [0;1;5;0;2] : expected %A actual %A -> %A" expected actual (expected = actual)
    

``Zero has one digit zero`` ()
``Collect digits of number`` ()
``No digits converts to 0`` ()
``Digits [0;1;5;0;2] converts to 1502`` ()