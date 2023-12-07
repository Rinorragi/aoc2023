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
    cards: int list
    handType: HandType
}

// Newline
let nl = "\n"

let solveHandType (hand : int list) = 
    let groupped = hand |> List.groupBy id |> List.sortByDescending (fun fs -> fs |> snd |> List.length) |> List.map snd
    if groupped.[0].Length = 5 then HandType.FiveKind
    elif groupped.[0].Length = 4 then HandType.FourKind
    elif groupped.[0].Length = 3 && groupped.[1].Length = 2 then HandType.FullHouse
    elif groupped.[0].Length = 3 then HandType.ThreeKind
    elif groupped.[0].Length = 2 && groupped.[1].Length = 2 then HandType.TwoPairs
    elif groupped.[0].Length = 2 then HandType.Pair
    else HandType.HighCard

let sortHands (hands : CamelHand list) =
    hands 
    |> List.groupBy (fun h -> h.handType) 
    |> List.sortByDescending (fst) 
    |> List.map (fun (_, someHands) -> 
        // Sort in reverse order to keep order of the earlier ones
        someHands
        |> List.sortByDescending (fun hand -> hand.cards[4])
        |> List.sortByDescending (fun hand -> hand.cards[3])
        |> List.sortByDescending (fun hand -> hand.cards[2])
        |> List.sortByDescending (fun hand -> hand.cards[1])
        |> List.sortByDescending (fun hand -> hand.cards[0]))
    |> List.concat

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> 
        let values = s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        let hand = 
            values.[0].ToCharArray() 
            |> Array.map (fun c -> 
                match c with 
                | 'A' -> 14
                | 'K' -> 13
                | 'Q' -> 12
                | 'J' -> 11
                | 'T' -> 10
                | _  -> int c - int '0')
            |> List.ofArray
        {
            bid = values.[1] |> int64
            cards = hand
            handType = solveHandType hand    
        })
    |> List.ofArray
    |> sortHands

let calculateTotalWinnings (hands: CamelHand list) = 
    let len = hands.Length
    hands
    |> List.rev
    |> List.mapi (fun i hand -> (int64(i) + 1L) * hand.bid)
    |> List.sum

let exampleGame = parseInput "./input/day07_example.txt"
let game = parseInput "./input/day07.txt"

exampleGame |> calculateTotalWinnings |> printfn "Example answer 1: %A"
game |> calculateTotalWinnings |> printfn "Answer 1: %A"