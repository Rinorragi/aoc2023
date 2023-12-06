// Macro to get running time
#time 

// Newline
let nl = "\n"

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> 
        s.Split(':', System.StringSplitOptions.RemoveEmptyEntries).[1])
    |> List.ofArray

let simulateRaces (times: int64 list) (distances: int64 list) =
    times 
    |> List.mapi (fun index raceTotalTime -> 
        let races = 
            [0L..raceTotalTime]
            |> List.map (fun chargeTime -> 
                let runningTime = raceTotalTime - chargeTime
                let runDistance = runningTime * chargeTime 
                match distances.[index] < runDistance with 
                | true -> Some(runDistance)
                | false -> None)
            |> List.choose id
        races)

let parseStringsToIntArrays (sArr : string list) =
    sArr 
    |> List.map (fun s -> 
        s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int64 
        |> List.ofArray)

let stringWithSpacesToInt (sInput: string list) = sInput |> List.map (fun s -> s.Replace(" ","")) |> List.map int64

let calculateResult (results: int64 list list) = 
    results
    |> List.map List.length
    |> List.fold (*) 1

let sExampleInput = parseInput "./input/day06_example.txt" 
let exampleRacesPart1 = sExampleInput |> parseStringsToIntArrays
let sInput = parseInput "./input/day06.txt" 
let racesPart1 = sInput |> parseStringsToIntArrays

simulateRaces exampleRacesPart1.[0] exampleRacesPart1.[1]
|> calculateResult
|> printfn "Example answer 1: %d"

simulateRaces racesPart1.[0] racesPart1.[1]
|> calculateResult
|> printfn "Answer 1: %A"

let exampleRacesPart2 = stringWithSpacesToInt sExampleInput
let racesPart2 = stringWithSpacesToInt sInput

simulateRaces [exampleRacesPart2.[0]] [exampleRacesPart2.[1]]
|> calculateResult
|> printfn "Example answer 2: %d"

simulateRaces [racesPart2.[0]] [racesPart2.[1]]
|> calculateResult
|> printfn "Answer 2: %d"
