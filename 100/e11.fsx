#load "../lib/scriptData.fsx"
let data = ScriptData.dataRows __SOURCE_FILE__ __SOURCE_DIRECTORY__

let splitToCells (txt : string) =
    txt.Split [|' '|]
    |> List.ofArray
    |> List.map int

let grid = data |> List.map splitToCells

// todo move to reusable lib - *or* - do the nested looping with seq?
let cartesian xs ys = 
    xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))

let innerSize = grid.Length - 4
let innerTopLeftSquare = cartesian [0..innerSize] [0..innerSize]
let innerLeftSquare    = cartesian [0..grid.Length - 1] [0..innerSize]
let innerTopSquare     = cartesian [0..innerSize] [0..grid.Length - 1]

let verticalProduct (x,y) =
    grid.[x].[y] *
    grid.[x + 1].[y] *
    grid.[x + 2].[y] *
    grid.[x + 3].[y]

let horizontalProduct (x,y) =
    grid.[x].[y] *
    grid.[x].[y + 1] *
    grid.[x].[y + 2] *
    grid.[x].[y + 3]    

let backslashDiagonalProduct (x,y) =
    grid.[x].[y] *
    grid.[x + 1].[y + 1] *
    grid.[x + 2].[y + 2] *
    grid.[x + 3].[y + 3]

let frontslashDiagonalProduct (x,y) =
    grid.[x + 3].[y] *
    grid.[x + 2].[y + 1] *
    grid.[x + 1].[y + 2] *
    grid.[x].[y + 3]

let crossMax =
    innerTopLeftSquare
    |> List.collect (fun cell -> [backslashDiagonalProduct cell; frontslashDiagonalProduct cell])
    |> List.max

let verticalMax =
    innerTopSquare
    |> List.map verticalProduct
    |> List.max

let horizontalMax =
    innerLeftSquare
    |> List.map horizontalProduct
    |> List.max

let totalMax =
    [crossMax; verticalMax; horizontalMax;]
    |> List.max

// todo find & do matrix library/tutorial

let cellZero = (0, 0)
printfn "test verticalProduct cellZero (1651104) = %A" (verticalProduct cellZero)
printfn "test horizontalProduct cellZero (34144) = %A" (horizontalProduct cellZero)
printfn "test backslashDiagonalProduct cellZero (279496) = %A" (backslashDiagonalProduct cellZero)
printfn "test frontslashDiagonalProduct cellZero (24468444) = %A" (frontslashDiagonalProduct cellZero)
printfn "real = %A" (totalMax)