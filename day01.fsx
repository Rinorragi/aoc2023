// Macro to get running time
#time 

// Newline
let nl = "\n"

let parseInput (filePath) stringFilter =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(fun (s : string) -> stringFilter s)
    |> Array.map(fun (s : string) ->
        s.ToCharArray() 
        |> Array.filter(fun c -> [|'0'..'9'|] |> Array.contains(c)) 
     )

let calibrationValues (rawInput: char array array) =
    rawInput |> Array.map(fun chars -> 
        [| chars[0]; chars |> Array.last |] 
        |> System.String 
        |> int)

// Example 1
parseInput "./input/day01_example.txt" (fun (s : string) -> s)
|> calibrationValues
|> Array.sum
|> printfn "Example answer: %d"
|> ignore

// Answer 1
parseInput "./input/day01.txt" (fun (s : string) -> s)
|> calibrationValues
|> Array.sum
|> printfn "Answer 1: %d"
|> ignore

let stringReplaceToInt (s: string) = 
    s.Replace("zero","0")
        .Replace("one","1")
        .Replace("two","2")
        .Replace("three","3")
        .Replace("four","4")
        .Replace("five","5")
        .Replace("six","6")
        .Replace("seven","7")
        .Replace("eight","8")
        .Replace("nine","9")

let findIndex (sArr : string array) (s : string) indexFinder sorter = 
    sArr 
    |> Array.map (fun sInt-> 
        let index = indexFinder s sInt
        match index with
        | -1 -> None
        |  _ -> Some(index, sInt))
    |> Array.choose id
    |> sorter fst
    |> Array.head 
    |> snd 
    |> stringReplaceToInt

let stringsToCharsFilter (s: string) =
    let sArr = 
        [|'0'..'9'|] 
        |> Array.map (fun c -> string c) 
        |> Array.append [|"one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"|]
    let firstIndex = findIndex sArr s (fun s sInt -> s.IndexOf(sInt)) Array.sortBy
    let lastIndex = findIndex sArr s (fun s sInt -> s.LastIndexOf(sInt)) Array.sortByDescending
    firstIndex + lastIndex

// Example 2
parseInput "./input/day01_example2.txt" stringsToCharsFilter
|> calibrationValues
|> Array.sum
|> printfn "Example 2 answer: %d"
|> ignore

// Answer 2
parseInput "./input/day01.txt" stringsToCharsFilter
|> calibrationValues
|> Array.sum
|> printfn "Answer 2: %d"
|> ignore
