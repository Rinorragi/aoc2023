// Macro to get running time
#time 

let nl = "\n"

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> 
        s.Split(nl+nl, System.StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map (fun mirror -> 
            mirror.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
            |> List.ofArray 
            |> List.map (fun s -> s.ToCharArray() |> List.ofArray ))
        |> List.ofArray

let rec transpose matrix = 
  match matrix with 
  | row::rows -> 
    match row with 
    | col::cols -> 
      let first: 'b List = List.map List.head matrix
      let rest = transpose (List.map List.tail matrix) 
      first :: rest
    | _ -> []
  | _ -> [] 

let checkReflection (tolerance: int) (mirror: char list list) (i:int) = 
    let indexesToCheck = [0..i] |> List.rev
    let diffAmount = 
        indexesToCheck
        |> List.map (fun k -> 
            let step = i + 1 + (i-k)
            if step >= mirror.Length then 0 
            else 
                let reflectionRow = mirror.[k]
                let row = mirror.[step]
                row 
                |> List.mapi (fun i c -> row.[i] <> reflectionRow.[i])
                |> List.filter id
                |> List.length)
        |> List.sum
    if diffAmount = tolerance then Some(i) else None

let findReflection (tolerance: int) (mirror: char list list)=
    let height = mirror.Length
    let reflectionRows = 
        [0..height - 2] 
        |> List.map (checkReflection tolerance mirror)
        |> List.choose id
    if reflectionRows.Length = 0 then -1 else reflectionRows.[0]

let reflectionCalculation (tolerance: int) (mirror: char list list) =
    let rowReflection = findReflection tolerance mirror
    if rowReflection >= 0
    then 
        (rowReflection + 1) * 100
    else 
        let transposedMirror = transpose mirror
        let colReflection = findReflection tolerance transposedMirror
        if colReflection = - 1 
        then 
            mirror |> List.map (fun s -> s |> Array.ofList |> System.String |> printfn "%s") |> ignore
            failwith "Failure with above mirror"
        (colReflection + 1)

let exampleMirrors = parseInput "./input/day13_example.txt" 

let mirrors = parseInput "./input/day13.txt" 

exampleMirrors |> List.map (reflectionCalculation 0) |> List.sum |> printfn "Example answer 1: %d"
mirrors |> List.map (reflectionCalculation 0) |> List.sum |> printfn "Answer 1: %d"

exampleMirrors |> List.map (reflectionCalculation 1) |> List.sum |> printfn "Example answer 2: %d"
mirrors |> List.map (reflectionCalculation 1) |> List.sum |> printfn "Answer 2: %d"