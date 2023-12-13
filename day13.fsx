// Macro to get running time
#time 

let nl = "\n"

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> 
        s.Split(nl+nl, System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map (fun mirror -> 
            mirror.Split(nl, System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray)

let rec transpose matrix = 
  match matrix with 
  | row::rows -> 
    match row with 
    | col::cols -> 
      let first = List.map List.head matrix
      let rest = transpose (List.map List.tail matrix) 
      first :: rest
    | _ -> []
  | _ -> [] 

let checkReflection (mirror: string list) (i:int) = 
    let indexesToCheck = [0..i] |> List.rev
    let isReflection = 
        indexesToCheck
        |> List.map (fun k -> 
            let step = i + 1 + (i-k)
            if step >= mirror.Length then true 
            else 
                let reflectionRow = mirror.[k]
                let row = mirror.[step]
                row = reflectionRow)
        |> List.forall (fun f -> f)
    if isReflection then Some(i) else None

let findReflection (mirror: string list) =
    let height = mirror.Length
    let reflectionRows = 
        [0..height - 2] 
        |> List.map (checkReflection mirror)
        |> List.choose id
    if reflectionRows.Length = 0 then -1 else reflectionRows.[0]

let reflectionCalculation (mirror: string list) =
    let rowReflection = findReflection mirror
    if rowReflection >= 0
    then 
        (rowReflection + 1) * 100
    else 
        let transposedMirror = 
            mirror 
            |> List.map (fun s -> s.ToCharArray() |> List.ofArray) 
            |> transpose
            |> List.map (fun cList -> 
                cList |> Array.ofList |> System.String)        
        let colReflection = findReflection transposedMirror
        if colReflection = - 1 
        then 
            mirror |> List.map (fun s -> printfn "%s" s) |> ignore
            failwith "Failure with above mirror"
        (colReflection + 1)

let exampleMirrors = parseInput "./input/day13_example.txt" 

let mirrors = parseInput "./input/day13.txt" 

exampleMirrors |> List.map reflectionCalculation |> List.sum |> printfn "Example answer 1: %d"
mirrors 
|> List.mapi (fun i sList -> 
    let midSum = reflectionCalculation sList
    midSum)
|> List.sum |> printfn "Answer 1: %d"