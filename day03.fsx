// Macro to get running time
#time 

// Newline
let nl = "\n"

type CharType = Number|Dot|Character

type gear = {
    x: int
    y: int
    op: char
}

type enginePart = {
    startX: int
    startY: int
    endX: int
    endY: int
    number: int
    gear: gear
}

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.mapi (fun xPos chars -> 
        chars 
        |> Array.mapi (fun yPos c -> 
            match c with 
            | '0' |'1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> (Number, c, xPos, yPos)
            | '.' -> (Dot, c, xPos, yPos)
            | _ -> (Character, c, xPos, yPos)))

let getEnginePart (input: (CharType * char * int * int) array array) (x: int) (yStart: int) =
    input.[x] |> Array.fold (fun (y: int) (charType, c, xPos, yPos) ->
        let result = 
            match yPos < yStart with 
            | true -> -1
            | false -> 
                match y with
                | -1 -> yPos // should be start of the number
                | _ -> 
                    match charType with
                    | Number -> if (y + 1 = yPos) then yPos else y
                    | Dot -> y
                    | Character -> y 
        result
    ) -1 

let checkSpecialCharacterNextToEngine (input: (CharType * char * int * int) array array) (x: int) (yStart: int) (yEnd: int) =
    let xMax = input.Length - 1
    let yMax = input.[0].Length - 1
    let yRange = 
        if yStart = 0 then [|yStart..yEnd + 1|]
        elif yEnd = yMax then [|yStart - 1..yEnd|]
        else [|yStart - 1..yEnd + 1|]
    let above = 
        if x = 0 then [||]  
        else  yRange |> Array.map (fun y ->  input.[x - 1].[y])
    let below =
        if x = xMax then [||]  
        else  yRange |> Array.map (fun y ->  input.[x + 1].[y])
    let currentRowStart = 
        if yStart = 0 then [||]
        else [|input.[x].[yStart - 1]|]
    let currentRowEnd = 
        if yEnd = yMax then [||]
        else [|input.[x].[yEnd + 1]|]
    let chars = 
        Array.append above below 
        |> Array.append currentRowStart 
        |> Array.append currentRowEnd
        |> Array.filter (fun (charType, c, xPos, yPos) -> charType = Character)
    if chars.Length = 0 then None
    else Some(chars.[0])
 

let getEnginePartValue (input: (CharType * char * int * int) array array) (x: int) (yStart: int) (yEnd: int) =
    let chars = 
        [|yStart..yEnd|] 
        |> Array.map (fun y -> 
            let (_, c, _, _) = input.[x].[y]
            c)
    new string(chars) |> int
    
let constructEngineParts (input: (CharType * char * int * int) array array) = 
    input |> Array.fold(fun (partState) row -> 
        let rowState = 
            row |> Array.fold(fun (rowPartState, skipToY) (charType, c, xPos, yPos) -> 
                if skipToY > yPos then (rowPartState, (skipToY))
                else 
                    match charType with
                    | Dot | Character -> (rowPartState, (yPos + 1))
                    | Number ->
                        let yEnd = getEnginePart input xPos yPos
                        let someCharacter = (checkSpecialCharacterNextToEngine input xPos yPos yEnd)
                        match someCharacter.IsSome with 
                        | false -> (rowPartState, (yEnd + 1))
                        | true -> 
                            let (_, opChar, gearX, gearY) = someCharacter.Value
                            ((Array.append rowPartState [|{
                                startX = xPos
                                endX = xPos
                                startY = yPos
                                endY = yEnd
                                number = getEnginePartValue input xPos yPos yEnd
                                gear = {
                                    x = gearX
                                    y = gearY
                                    op = opChar
                                }
                            }|]), (yEnd + 1))
                    
            ) ([||],0)
        (Array.append partState (fst rowState))
    ) ([||]) 

let findGears (parts : enginePart array) =
    parts 
    |> Array.groupBy (fun part -> part.gear)
    |> Array.map snd
    |> Array.filter (fun set -> set.Length > 1)
    |> Array.map (fun set -> set.[0].gear)

let calculateGearRatio (parts: enginePart array) (gears: gear array) = 
    parts
    |> Array.filter (fun f -> gears |> Array.contains f.gear)
    |> Array.groupBy (fun part -> part.gear)
    |> Array.map (fun (_, gearParts) -> 
        gearParts.[0].number * gearParts.[1].number)

let exampleParts = parseInput "./input/day03_example.txt" |> constructEngineParts
let parts = parseInput "./input/day03.txt" |> constructEngineParts

exampleParts
|> Array.sumBy (fun f -> f.number)
|> printfn "Example answer 1: %A"
|> ignore

parts
|> Array.sumBy (fun f -> f.number)
|> printfn "Answer 1: %A"
|> ignore

calculateGearRatio exampleParts (findGears exampleParts)
|> Array.sum
|> printfn "Example answer 2: %A"

calculateGearRatio parts (findGears parts)
|> Array.sum
|> printfn "Answer 2: %A"