// Macro to get running time
#time 

// Newline
let nl = "\n"

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)|> Array.map int |> List.ofArray)
    |> List.ofArray 

let rec getPredictionValue  (historyRow: int list) =
    let pairDiffs = 
        historyRow |> List.pairwise |> List.map (fun (first, second) -> second - first)
    let predictionDiff = 
        match (pairDiffs |> List.forall(fun i -> i = 0)) with
        | true -> 0
        | false -> getPredictionValue pairDiffs
    historyRow |> List.last |> (+) predictionDiff

let instabilitySensorHistoryExamples = parseInput "./input/day09_example.txt"
let instabilitySensorHistories = parseInput "./input/day09.txt"
instabilitySensorHistoryExamples |> List.map getPredictionValue |> List.sum |> printfn "Example answer 1: %A"
instabilitySensorHistories |> List.map getPredictionValue |> List.sum |> printfn "Answer 1: %A"
