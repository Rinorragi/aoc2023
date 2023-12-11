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
    |> List.map int64
    |> Set.ofList

let getColsAndRowsToAppend (universe: char list list) =
    (getRowsToAppend universe, transpose universe |> getRowsToAppend)

let toCoord (rowLength: int64) (num: int64) = num / rowLength, num % rowLength
let manhattanDistance (x1, y1) (x2, y2) = abs(x1 - x2) + abs (y1 - y2)
let adjustUniverseAge (rows: int64 Set) (cols: int64 Set) (expansion: int64) (x1, y1) (x2, y2) =
    let rowSpace = (if x1 < x2 then [x1..x2] else [x2..x1]) |> Set.ofList
    let colSpace = (if y1 < y2 then [y1..y2] else [y2..y1]) |> Set.ofList
    let rowsToAdd = Set.intersect rowSpace rows |> Set.count |> int64
    let colsToAdd = Set.intersect colSpace cols |> Set.count |> int64
    let yPair = if y1 < y2 then y1, y2 + (colsToAdd * expansion) else y1 + (colsToAdd * expansion),y2
    let xPair = if x1 < x2 then x1,x2 + (rowsToAdd * expansion) else x1 + (rowsToAdd * expansion),x2
    (fst xPair, fst yPair), (snd xPair, snd yPair)

let galaxyPaths (expansion: int64) (universe: char list list) =
    let (rows, cols) = getColsAndRowsToAppend universe
    let stringVerse = universe |> List.map (fun cList -> cList |> List.toArray |> System.String)
    let rowLength = stringVerse.[0].Length |> int64
    let theOneString = stringVerse |> List.fold (fun state newRow -> state + newRow) ""
    let galaxyIndexes = 
        theOneString.ToCharArray() 
        |> List.ofArray
        |> List.mapi (fun i c -> 
            match c with 
            | '#' -> Some(i)
            | _ -> None)
        |> List.choose id
        |> List.map int64
    let galaxyPairs = 
        List.allPairs galaxyIndexes galaxyIndexes 
        |> List.distinct
        |> List.filter (fun (i1, i2) -> i1 <> i2 && i1 < i2) // avoid all duplicates
    let distances =
        galaxyPairs
        |> List.map (fun (i1, i2) -> 
            let xy1 = toCoord rowLength (i1)
            let xy2 = toCoord rowLength (i2)
            let (agedXY1, agedXY2) = adjustUniverseAge rows cols expansion xy1 xy2
            manhattanDistance agedXY1 agedXY2)
    distances

let exampleUniverse = parseInput "./input/day11_example.txt" 
exampleUniverse |> galaxyPaths 1 |> List.sum |> printfn "Example answer 1: %d"
let universe = parseInput "./input/day11.txt" 
universe |> galaxyPaths 1 |> List.sum |> printfn "Answer 1: %d"

exampleUniverse |> galaxyPaths 9 |> List.sum |> printfn "Example answer 2 (10): %d"
exampleUniverse |> galaxyPaths 99 |> List.sum |> printfn "Example answer 2 (100): %d"
universe |> galaxyPaths 999999 |> List.sum |> printfn "Answer 2: %d"