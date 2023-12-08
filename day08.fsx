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

let rec traverse (lrSeq: char array) (instructions: Map<string,Instruction>) (endKey: string) (key: string) (index: int64) =
    if (key = endKey) then index
    else 
        let modIndex = if index >= lrSeq.Length then (index % int64(lrSeq.Length)) else index
        if modIndex > int64(lrSeq.Length) - 1L then printfn "IndexOOF incoming with key %s at step %d" key index 
        let newKey = 
            match lrSeq.[(int(modIndex))] with
            | 'R' -> instructions[key].right
            | 'L' -> instructions[key].left
            | _ -> raise (System.ArgumentException("Unknown char"))
        //printfn "Key %s -> Key %s, Index: %d " key newKey index
        traverse lrSeq instructions endKey newKey (index + 1L)

let ((example1lrSeq),(example1instructions: Map<string,Instruction>)) = parseInput "./input/day08_example.txt"
let ((example2lrSeq),(example2instructions: Map<string,Instruction>))  = parseInput "./input/day08_example2.txt"
let (lrSeq, instructions) = parseInput "./input/day08.txt"

let startKey = "AAA"
let endKey = "ZZZ"

traverse (example1lrSeq) example1instructions endKey startKey 0 |> printfn "Example answer 1.1: %A"
traverse (example2lrSeq) example2instructions endKey startKey 0 |> printfn "Example answer 1.2: %A"
traverse (lrSeq) instructions endKey startKey 0 |> printfn "Answer 1: %A"
