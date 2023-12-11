// Macro to get running time
#time 

let nl = "\n"

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> List.map (fun s -> s.ToCharArray() |> List.ofArray)

let isEmptySpace (cList: char list) =
    cList |> List.forall (fun c -> c = '.')

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

let getRowsToAppend (universe: char list list) =
    universe 
    |> List.mapi (fun i cList ->  
        match isEmptySpace cList with 
        | true -> Some(i)
        | _ -> None)
    |> List.choose id

let getColsAndRowsToAppend (universe: char list list) =
    (getRowsToAppend universe, transpose universe |> getRowsToAppend)      

let appendEmptySpaceToRows (universe: char list list) (rows: int list) =
    universe
    |> List.fold (fun (index, newUniverse) row ->
        match rows |> List.contains index with 
        | true ->  (index + 1, (List.append newUniverse [row; row]))
        | false -> (index + 1, (List.append newUniverse [row]))
    ) (0, [])
    |> snd

let appendUniverse (universe: char list list) (rows: int list, cols: int list) =
    let newUniverse = appendEmptySpaceToRows universe rows
    let finalUniverse = appendEmptySpaceToRows (transpose newUniverse) cols
    transpose finalUniverse

let printUniverse (universe: char list list) = 
    universe |> List.map (fun cList -> cList |> List.toArray |> System.String |> printfn "%s")

let parseUniverse (filePath: string) = 
    let universe = parseInput filePath
    appendUniverse universe (getColsAndRowsToAppend universe) 

let toCoord rowLength num = num / rowLength, num % rowLength
let manhattanDistance (x1, y1) (x2, y2) = abs(x1 - x2) + abs (y1 - y2)

let galaxyPaths (universe: char list list) =
    let stringVerse = universe |> List.map (fun cList -> cList |> List.toArray |> System.String)
    let rowLength = stringVerse.[0].Length
    let theOneString = stringVerse |> List.fold (fun state newRow -> state + newRow) ""
    let galaxyIndexes = 
        theOneString.ToCharArray() 
        |> List.ofArray
        |> List.mapi (fun i c -> 
            match c with 
            | '#' -> Some(i)
            | _ -> None)
        |> List.choose id
    let galaxyPairs = 
        List.allPairs galaxyIndexes galaxyIndexes 
        |> List.distinct
        |> List.filter (fun (i1, i2) -> i1 <> i2 && i1 < i2) // avoid all duplicates
    let distances =
        galaxyPairs
        |> List.map (fun (i1, i2) -> 
            let xy1 = toCoord rowLength (i1)
            let xy2 = toCoord rowLength (i2)
            manhattanDistance xy1 xy2)
    distances

let exampleUniverse = parseUniverse "./input/day11_example.txt" 
exampleUniverse |> galaxyPaths |> List.sum |> printfn "Example answer 1: %d"
let universe = parseUniverse "./input/day11.txt" 
universe |> galaxyPaths |> List.sum |> printfn "Answer 1: %d"