// Macro to get running time
#time 

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    |> fun s -> s.Replace("\r","").Replace("\n", "")
    |> fun s -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray 

let hasher (input: string list) =
    input
    |> List.map (fun s -> 
        s.ToCharArray() 
        |> List.ofArray
        |> List.fold (fun state c -> 
            ((state + (int c)) * 17) % 256 
        ) 0 )
    |> List.sum

parseInput "./input/day15_example.txt" |> hasher |> printfn "Example answer 1: %d" 
parseInput "./input/day15.txt" |> hasher |> printfn "Answer 1: %d" 
    