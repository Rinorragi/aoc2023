// Macro to get running time
#time 

type Lens = {
    label: string
    focalLength: int    
}

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

let exampleInput = parseInput "./input/day15_example.txt" 
let input = parseInput "./input/day15.txt" 

exampleInput |> p1Solver |> printfn "Example answer 1: %d" 
input |> p1Solver |> printfn "Answer 1: %d" 
    
// 256 boxes 0 --> 255 light goes through 0 to 255
// each box has series of lenses (focal length 1 to 9)
// steps separated with ,
// hash(label meaning the chars) = box
// - remove lens with label and move others forward
// = add lens with label 
//   - number tells the focal length
//   - if there is already a same labeled replace it at same position
let p2Solver (steps: string list) = 
    let (boxes: Lens list list) = [0..255] |> List.map (fun box -> []) 
    let lensedBoxes = 
        steps
        |> List.fold (fun boxState step -> 
            let name = step.Split([|'=';'-'|]).[0]
            let value = 
                if step.Contains('-') then -1
                else step.Split('=').[1] |> int
            let lens = { label = name; focalLength = value }
            let boxNumber = hasher name        
            boxState 
            |> List.mapi (fun i box ->
                match i = boxNumber, value, (box |> List.exists(fun b -> b.label = name)) with
                | false, _, _ -> box // wrong box
                | true, -1, _ ->
                    // Right box with remove
                    box |> List.filter (fun b -> b.label <> name)
                | true, _, false ->
                    // Right box without replacement      
                    box @ [lens]
                | true, _, true -> 
                    // right box with replacement
                    box |> List.map (fun l -> if l.label = lens.label then lens else l))
        ) boxes
    lensedBoxes
    |> List.mapi (fun boxNumber box ->
        box 
        |> List.mapi (fun i lens -> (boxNumber+1) * (i+1) * lens.focalLength)
        |> List.sum)
    |> List.sum

exampleInput |> p2Solver |> printfn "Example answer 2: %A"
input |> p2Solver |> printfn "Example answer 2: %A"