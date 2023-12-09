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

let rec getPredictionValue appender (historyRow: int list) =
    let pairDiffs = 
        historyRow |> List.pairwise |> List.map (fun (first, second) -> second - first)
    let predictionDiff = 
        match (pairDiffs |> List.forall(fun i -> i = 0)) with
        | true -> 0
        | false -> getPredictionValue appender pairDiffs
    historyRow |> appender predictionDiff

let part1Appender diffValue list  = list |> List.last |> (+) diffValue
let part1Solver values text = 
    values 
    |> List.map (fun f -> getPredictionValue part1Appender f) 
    |> List.sum 
    |> printfn text

let part2Appender diffValue list  = list |> List.head |> (fun f -> f - diffValue)
let part2Solver values text = 
    values 
    |> List.map (fun f -> getPredictionValue part2Appender f) 
    |> List.sum 
    |> printfn text

let instabilitySensorHistoryExamples = parseInput "./input/day09_example.txt"
let instabilitySensorHistories = parseInput "./input/day09.txt"
part1Solver instabilitySensorHistoryExamples "Example answer 1: %A"
part1Solver instabilitySensorHistories "Answer 1: %A"
part2Solver instabilitySensorHistoryExamples "Example answer 2: %A"
part2Solver instabilitySensorHistories "Answer 2: %A"