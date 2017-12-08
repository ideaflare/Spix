let upToNineteen =
    [
        0, "";
        1, "one";
        2, "two";
        3, "three";
        4, "four";
        5, "five";
        6, "six";
        7, "seven";
        8, "eight";
        9, "nine";
        10, "ten";
        11, "eleven";
        12, "twelve";
        13, "thirteen";
        14, "fourteen";
        15, "fifteen";
        16, "sixteen";
        17, "seventeen";
        18, "eighteen";
        19, "nineteen";    
    ] |> Map.ofList

let upToHundred =
    [
        0, "";
        20, "twenty";        
        30, "thirty";        
        40, "forty";        
        50, "fifty";        
        60, "sixty";        
        70, "seventy";        
        80, "eighty";        
        90, "ninety";        
    ] |> Map.ofList

let hundred = "hundred"

let wordUnderHundred n =
    if n < 20 then upToNineteen.[n]
    else
        let tenPart = (n / 10) * 10
        let tenWord = upToHundred.[tenPart]
        let lastDigit = n - tenPart
        let lastWord = upToNineteen.[lastDigit]
        tenWord + lastWord

let wordUnderThousand n =
    let hundredDigit = n / 100
    let hundredWord = wordUnderHundred (hundredDigit)
    let underHundredPart = n - (hundredDigit * 100)
    if underHundredPart = 0 then hundredWord + hundred
    else
        let underHundredWord = (wordUnderHundred underHundredPart)
        hundredWord + hundred + "and" + underHundredWord


let word n =
    if n < 100 then wordUnderHundred n
    else if n = 1000 then "one" + "thousand"
    else wordUnderThousand n

let sumCharacters n =
    [1..n]
    |> List.sumBy (fun n ->
        let text = word n
        printfn "n: %d %A" n text
        (word n).Length)

printfn "word 55 fiftyfive = %A" (word 55)
printfn "word 73 seventythree = %A" (word 73)
printfn "word 20 twenty = %A" (word 20)

printfn "word 342 threehundredandfourtytwo = %A" (word 342)
printfn "word 115 onehundredandfifteen = %A" (word 115)

printfn "test 5 (19) = %A" (sumCharacters 5)
printfn "real = %A" (sumCharacters 1000)