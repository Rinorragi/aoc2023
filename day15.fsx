// Macro to get running time
#time 

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    |> fun s -> s.Replace("\r","").Replace("\n", "")
    |> fun s -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray 

let hasher (s: string) =
    s.ToCharArray() 
    |> List.ofArray
    |> List.fold (fun state c -> 
        ((state + (int c)) * 17) % 256 
    ) 0
let p1Solver (input: string list) = input |> List.map hasher |> List.sum

parseInput "./input/day15_example.txt" |> p1Solver |> printfn "Example answer 1: %d" 
parseInput "./input/day15.txt" |> p1Solver |> printfn "Answer 1: %d" 
    