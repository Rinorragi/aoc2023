// Macro to get running time
#time 

type HandType = 
    | FiveKind = 7
    | FourKind = 6
    | FullHouse = 5
    | ThreeKind = 3
    | TwoPairs = 2
    | Pair = 1
    | HighCard = 0

type CamelHand = {
    bid: int64
    cards: char list
    handType: HandType
}

// Newline
let nl = "\n"

let countJokers (jokers: bool) (hand: char list) = 
    if jokers then hand |> List.filter (fun c -> c = 'J') |> List.length
    else 0

let groubCardsInHand (jokers: bool) (hand: char list) = 
    let groupped = hand |> List.groupBy id |> List.sortByDescending (fun fs -> fs |> snd |> List.length)
    if jokers then groupped |> List.filter (fun (c, cList) -> c <> 'J') |> List.map snd 
    else groupped |> List.map snd

let solveHandType (jokers: bool) (hand : char list) = 
    let iJokers = countJokers jokers hand 
    let groupped = groubCardsInHand jokers hand
    if iJokers = 5 || groupped.[0].Length + iJokers = 5 then HandType.FiveKind
    elif groupped.[0].Length + iJokers = 4 then HandType.FourKind
    elif groupped.[0].Length + iJokers = 3 && groupped.[1].Length = 2 then HandType.FullHouse
    elif groupped.[0].Length + iJokers = 3 then HandType.ThreeKind
    elif groupped.[0].Length = 2 && groupped.[1].Length + iJokers = 2 then HandType.TwoPairs
    elif groupped.[0].Length + iJokers = 2 then HandType.Pair
    else HandType.HighCard

let sortHands charValueSolver (hands : CamelHand list) =
    hands 
    |> List.groupBy (fun h -> h.handType) 
    |> List.sortByDescending (fst) 
    |> List.map (fun (_, someHands) -> 
        // Sort in reverse order to keep order of less significant card order
        someHands
        |> List.sortByDescending (fun hand -> charValueSolver hand.cards[4])
        |> List.sortByDescending (fun hand -> charValueSolver hand.cards[3])
        |> List.sortByDescending (fun hand -> charValueSolver hand.cards[2])
        |> List.sortByDescending (fun hand -> charValueSolver hand.cards[1])
        |> List.sortByDescending (fun hand -> charValueSolver hand.cards[0]))
    |> List.concat

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)

let charToInt (jokers: bool) (c: char) =
    match c with 
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> if jokers then 1 else 11
    | 'T' -> 10
    | _  -> int c - int '0'

let stringsToCamelHands (jokers: bool) (sHands : string array) =
    sHands
    |> Array.map (fun s -> 
        let values = s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        let hand = 
            values.[0].ToCharArray() 
            |> List.ofArray
        {
            bid = values.[1] |> int64
            cards = hand
            handType = solveHandType jokers hand    
        })
    |> List.ofArray
    |> sortHands (charToInt jokers)

let calculateTotalWinnings (hands: CamelHand list) = 
    hands
    |> List.rev
    |> List.mapi (fun i hand -> (int64(i) + 1L) * hand.bid)
    |> List.sum

let exampleGame = parseInput "./input/day07_example.txt"
let game = parseInput "./input/day07.txt"

exampleGame |> stringsToCamelHands false |> calculateTotalWinnings |> printfn "Example answer 1: %A"
game |> stringsToCamelHands false |> calculateTotalWinnings |> printfn "Answer 1: %A"


exampleGame |> stringsToCamelHands true |> calculateTotalWinnings |> printfn "Example answer 2: %A"
game |> stringsToCamelHands true |> calculateTotalWinnings |> printfn "Answer 2: %A"