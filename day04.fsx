// Macro to get running time
#time 

// Newline
let nl = "\n"

type Card = {
    id: int 
    winningNumbers : int array
    myNumbers : int array
}

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s ->
        {
            id = s.Split(':').[0].Split(' ', System.StringSplitOptions.RemoveEmptyEntries).[1] |> int
            winningNumbers = s.Split(':').[1]
                .Split('|').[0]
                .Split(' ', System.StringSplitOptions.RemoveEmptyEntries) 
                |> Array.map int
            myNumbers = s.Split(':').[1]
                .Split('|').[1]
                .Split(' ', System.StringSplitOptions.RemoveEmptyEntries) 
                |> Array.map int
        } 
    )

let calculateWins (card: Card) =
    let foundWinningNumbers = 
        Set.intersect (card.winningNumbers |> Set.ofArray) (card.myNumbers |> Set.ofArray)
        |> Set.toArray
    foundWinningNumbers.Length

let winsToPow (wins: int) =
    match wins with 
    | 0 -> 0
    | _ -> pown 2 (wins - 1)

let rec wonCardsForCard (cards: Card array) (card: Card) (cardsWon: Card array) =
    let wins = calculateWins card
    match wins with
    | 0 -> [||]
    | _ -> 
        let wonCards : Card array = 
            [|card.id + 1 .. card.id + wins|]
            |> Array.map (fun cardId -> 
                cards.[cardId - 1])
        let moreCards : Card array = 
            wonCards 
            |> Array.map (fun card -> wonCardsForCard cards card cardsWon)
            |> Array.concat
        Array.concat [|wonCards; moreCards|]

let exampleCards = parseInput "./input/day04_example.txt" 
let cards = parseInput "./input/day04.txt" 

exampleCards
|> Array.map calculateWins
|> Array.map winsToPow
|> Array.sum
|> printfn "Example answer 1: %d"

cards
|> Array.map calculateWins
|> Array.map winsToPow
|> Array.sum
|> printfn "Answer 1: %d"

exampleCards
|> Array.map (fun card -> wonCardsForCard exampleCards card [||])
|> Array.concat
|> Array.length
|> (+) exampleCards.Length
|> printfn "Example answer 2: %d"

cards
|> Array.map (fun card -> wonCardsForCard cards card [||])
|> Array.concat
|> Array.length
|> (+) cards.Length
|> printfn "Answer 2: %d"