// Type Unknown used to avoid FS0025 (Incomplete pattern match). Refactor with F# 4.1 Result<'TSuccess, 'TError> type.

type Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace | UnknownFace
type Suit = Club | Diamond | Heart | Spade | UnknownSuit

type Card = {face : Face; suit : Suit}

type Rank = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush

type Hand = {rank: Rank; rankValue: Face; highCard: Face}

let face {face=face; suit=_} = face
let suit {face=_; suit=suit} = suit

let flush cards = 
    let firstSuit = Seq.tryHead cards |> Option.map suit
    Seq.forall (fun card -> Some(suit card) = firstSuit) cards

let sortedFaces = List.map face >> List.sort

let rank cards =
    match sortedFaces cards with
    | [Ten;Jack;Queen;King;Ace] when flush cards 
        -> RoyalFlush, []

    | [Two;Three;Four;Five;Ace] as straight when flush cards
         -> StraightFlush, straight |> List.sortDescending

    | [a;b;c;d;maxFace] as straight when flush cards && a < b && b < c && c < d && d < maxFace && compare maxFace a = 4
        -> StraightFlush, straight |> List.sortDescending

    | [f1;f2;f3;maxFace;hc] | [hc;f1;f2;f3;maxFace] when f1 = f2 && f2 = f3 && f3 = maxFace 
        -> FourOfAKind, [maxFace;hc]

    | [t1;t2;tMax;p1;pMax] | [p1;pMax;t1;t2;tMax] when t1 = t2 && t2 = tMax && p1 = pMax 
        -> FullHouse, [tMax;pMax]
        
    | hc when flush cards 
        -> Flush, hc |> List.sortDescending
    
    | [Two;Three;Four;Five;Ace] as straight 
        -> Straight, straight |> List.sortDescending
    
    | [a;b;c;d;maxFace] as straight when a < b && b < c && c < d && d < maxFace && compare maxFace a = 4
        -> Straight, straight |> List.sortDescending
    
    | [t1;t2;t3;hc1;hc2] | [hc1;t1;t2;t3;hc2] | [hc1;hc2;t1;t2;t3] when t1 = t2 && t2 = t3 ->
        ThreeOfAKind, [t3;hc2;hc1]
    
    | [a1;a2;b1;b2;hc] | [a1;a2;hc;b1;b2] | [hc;a1;a2;b1;b2] when a1 = a2 && b1 = b2 
        -> TwoPair, [max a1 b1; min a1 b1;hc]
    
    | [p1;p2;hc1;hc2;hc3] | [hc1;p1;p2;hc2;hc3] | [hc1;hc2;p1;p2;hc3] | [hc1;hc2;hc3;p1;p2] when p1 = p2
        -> OnePair, [p1;hc3;hc2;hc1]
    
    | hc -> HighCard, hc |> List.sortDescending

let parseFace =
    function
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six 
    | '7' -> Seven 
    | '8' -> Eight 
    | '9' -> Nine
    | 'T' -> Ten
    | 'J' -> Jack 
    | 'Q' -> Queen 
    | 'K' -> King 
    | 'A' -> Ace
    | _ -> UnknownFace

let parseSuit =
    function
    | 'C' -> Club
    | 'D' -> Diamond 
    | 'H' -> Heart 
    | 'S' -> Spade
    | _ -> UnknownSuit

let parseCard cardText =
    let faceText = cardText |> Seq.head
    let suitText = cardText |> Seq.skip 1 |> Seq.head
    {face = parseFace faceText; suit = parseSuit suitText }
    
let playerOneWin (row:string) =
    let cards = row.Split(' ') |> Seq.map parseCard |> List.ofSeq
    let p1 = List.take 5 cards
    let p2 = List.skip 5 cards |> List.take 5
    let rank1, rankHigh1 = rank p1
    let rank2, rankHigh2 = rank p2
    rank1 > rank2 ||
    (rank1 = rank2 && rankHigh1 > rankHigh2)

let testPlayer2WinHigherPair = not (playerOneWin "5H 5C 6S 7S KD 2C 3S 8S 8D TD")
let testPlayer1WinHigherCard = playerOneWin "5D 8C 9S JS AC 2C 5C 7D 8S QH"
let testPlayer2WinFlushDiamond = not (playerOneWin "2D 9C AS AH AC 3D 6D 7D TD QD")
let testPlayer1WinSameRankHighCard = playerOneWin "4D 6S 9H QH QC 3D 6D 7H QD QS"
let testPlayer1WinSameRankHigherRank = playerOneWin "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"

printfn "Player two higher pair wins (%b)" (testPlayer2WinHigherPair)
printfn "Player one higher card wins (%b)" (testPlayer1WinHigherCard)
printfn "Player two flush diamond wins (%b)" (testPlayer2WinFlushDiamond)
printfn "Player one same rank, high card wins (%b)" (testPlayer1WinSameRankHighCard)
printfn "Player one same rank, higher rank wins (%b)" (testPlayer1WinSameRankHigherRank)

#load "../lib/scriptData.fsx"
let data = ScriptData.dataRows __SOURCE_FILE__ __SOURCE_DIRECTORY__
let player1Wins = data |> Seq.filter playerOneWin |> Seq.length

printfn "real = %A" player1Wins