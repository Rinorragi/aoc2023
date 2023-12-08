// Macro to get running time
#time 

type Instruction = {
    location: string
    left: string
    right: string
}

// Newline
let nl = "\n"

let parseInput (filePath) =
    let input = 
        System.IO.File.ReadAllText filePath
        // Replace CRLF to only LF (copy+paste and input in different format)
        |> fun s -> s.Replace("\r\n", nl)
        |> fun s -> s.Split(nl+nl, System.StringSplitOptions.RemoveEmptyEntries)
    let lrSeq = input.[0].Trim().ToCharArray()
    let instructions = 
        input.[1].Split(nl, System.StringSplitOptions.RemoveEmptyEntries)  
        |> Array.map (fun (ins: string) -> 
            let instruction = 
                {
                    location = ins.Substring(0, 3)
                    left = ins.Substring(7, 3)
                    right = ins.Substring(12, 3)
                }
            (instruction.location, instruction))
        |> Map
    (lrSeq, instructions)

let takeStep (lrSeq: char array) (lrSeqLength: int) (instructions: Map<string,Instruction>) (key: string) (index: int) = 
    let newKey = 
        match lrSeq.[index % lrSeqLength] with
        | 'R' -> instructions[key].right
        | 'L' -> instructions[key].left
        | _ -> raise (System.ArgumentException("Unknown char"))
    (newKey, (index + 1))

let rec traverse (lrSeq: char array) (lrSeqLength: int) (instructions: Map<string,Instruction>) (endKeys: string Set) (key: string) (index: int) =
    if (endKeys.Contains(key)) then index
    else 
        let (newKey, newIndex) = takeStep lrSeq lrSeqLength instructions key index
        traverse lrSeq lrSeqLength instructions endKeys newKey newIndex

let multiTraverse (lrSeq: char array) (lrSeqLength: int) (instructions: Map<string,Instruction>) (endKeys: string Set) (keys: string Set) =
    let rec lcm (a: int64) (b: int64) = if b = 0L then a else (lcm b (a % b))
    keys 
    |> Set.map (fun key -> traverse lrSeq lrSeqLength instructions endKeys key 0)
    |> Set.toList
    |> List.map int64
    |> List.reduce (fun a b -> (a * b) / (lcm a b))

let getKeys (instructions: Map<string,Instruction>) =
    let getKey c = instructions.Keys |> Seq.filter (fun s -> s.Chars(2) = c) |> Set.ofSeq
    let startKeys = getKey 'A'
    let endKeys = getKey 'Z'
    (startKeys, endKeys)

let ((example1lrSeq),(example1instructions: Map<string,Instruction>)) = parseInput "./input/day08_example.txt"
let ((example2lrSeq),(example2instructions: Map<string,Instruction>)) = parseInput "./input/day08_example2.txt"
let ((example3lrSeq),(example3instructions: Map<string,Instruction>)) = parseInput "./input/day08_example3.txt"
let (lrSeq, instructions) = parseInput "./input/day08.txt"

let startKey = "AAA"
let endKeySetPart1 = Set.empty.Add("ZZZ")

// Part 1
traverse (example1lrSeq) (example1lrSeq.Length) example1instructions endKeySetPart1 startKey 0 |> printfn "Example answer 1.1: %A"
traverse (example2lrSeq) (example2lrSeq.Length) example2instructions endKeySetPart1 startKey 0 |> printfn "Example answer 1.2: %A"
traverse (lrSeq) (lrSeq.Length) instructions endKeySetPart1 startKey 0 |> printfn "Answer 1: %A"

// Part 2
let (example3StartKeys, example3EndKeys) = getKeys example3instructions
let (startKeys, endKeys) = getKeys instructions
multiTraverse example3lrSeq (example3lrSeq.Length) example3instructions example3EndKeys example3StartKeys |> printfn "Example answer 2.1: %A"
multiTraverse lrSeq (lrSeq.Length) instructions endKeys startKeys |> printfn "Answer 2: %A"