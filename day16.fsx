// Macro to get running time
#time 

let nl = "\n"
type Direction =
    | North
    | West
    | South
    | East

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> 
        s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray 
        |> List.map (fun s -> s.ToCharArray() |> List.ofArray)

let moveBeam (dir: Direction, beamX:int, beamY:int) = 
    match dir with
    | North -> (North, beamX, beamY - 1)
    | South -> (South, beamX, beamY + 1)
    | West -> (West, beamX - 1, beamY)
    | East -> (East, beamX + 1, beamY)

let rotateBeam90DegreesAndMove (dir: Direction, beamX:int, beamY:int) (c: char) =
    match c, dir with 
    | '/', North -> moveBeam (East, beamX, beamY)
    | '/', South -> moveBeam (West, beamX, beamY)
    | '/', West -> moveBeam (South, beamX, beamY)
    | '/', East -> moveBeam (North, beamX, beamY)
    | '\\', North -> moveBeam (West, beamX, beamY)
    | '\\', South -> moveBeam (East, beamX, beamY)
    | '\\', West -> moveBeam (North, beamX, beamY)
    | '\\', East -> moveBeam (South, beamX, beamY)
    | _, _ -> failwith "Unknown 90 degrees rotation"

let mergeEnergyMaps (a :  Map<(int * int), Direction list>) (b :  Map<(int * int), Direction list>) =
    Map.fold (fun s k v ->
        match Map.tryFind k s with
        | Some v' -> Map.add k (v @ v' |> List.distinct) s
        | None -> Map.add k v s) a b

let rec beamMeUpScotty (dir: Direction, beamX:int, beamY:int) (grid: char list list) (energized: Map<(int * int), Direction list>) =
    // If beam escapes contraption stop
    if  beamX < 0 
        || beamY < 0 
        || beamY >= grid.Length
        || beamX >= grid.[0].Length
        || ((energized |> Map.containsKey (beamX,beamY)) && (energized.[(beamX, beamY)] |> List.contains (dir))) // prevent infinite loops
    then energized
    else
        let newEnergization = 
            if energized.ContainsKey (beamX, beamY)
            then 
                let oldOne = energized.[(beamX, beamY)]
                energized.Remove((beamX, beamY)).Add((beamX, beamY), oldOne @ [dir])
            else 
                energized.Add((beamX, beamY), [dir])
                
        match grid.[beamY].[beamX], dir with
        | '|', North
        | '|', South
        | '-', West
        | '-', East
        | '.', _ -> beamMeUpScotty (moveBeam (dir, beamX, beamY)) grid newEnergization
        | '/', _
        | '\\', _ -> beamMeUpScotty (rotateBeam90DegreesAndMove (dir,beamX,beamY) grid.[beamY].[beamX]) grid newEnergization
        | '|', West
        | '|', East ->  
            let up = beamMeUpScotty (moveBeam (North, beamX, beamY)) grid newEnergization
            // down
            beamMeUpScotty (moveBeam (South, beamX, beamY)) grid up
        | '-', North
        | '-', South ->
            let left = beamMeUpScotty (moveBeam (West, beamX, beamY)) grid newEnergization
            // right
            beamMeUpScotty (moveBeam (East, beamX, beamY)) grid left
        | _ -> failwith "Unknown grid event"

let mapLength (energyMap: Map<(int * int), Direction list>) =
    energyMap
    |> Map.toList
    |> List.map (fun ((x,y), _) -> (x,y))
    |> List.distinct
    |> List.length

let beamify (grid: char list list) =
    beamMeUpScotty (East, 0,0) grid Map.empty
    |> mapLength

let allTheBeams (grid: char list list) =
    let yMax = grid.Length - 1
    let xMax = grid.[0].Length - 1
    let yMaps =
        [0..yMax]
        |> List.map(fun y -> 
            let easts = 
                beamMeUpScotty (East, 0, y) grid Map.empty
                |> mapLength
            let wests = 
                beamMeUpScotty (West, xMax, y) grid Map.empty
                |> mapLength
            [easts;wests])
        |> List.concat
    let xMaps =
        [0..xMax]
        |> List.map(fun x -> 
            let souths = 
                beamMeUpScotty (South, x, 0) grid Map.empty
                |> mapLength
            let norths = 
                beamMeUpScotty (North, x, yMax) grid Map.empty
                |> mapLength
            [souths;norths])
        |> List.concat
    // Return max
    yMaps @ xMaps |> List.max
    

let exampleInput = parseInput "./input/day16_example.txt" 
let input = parseInput "./input/day16.txt" 
exampleInput |> beamify |> printfn "Example answer 1: %d"
input |> beamify |> printfn "Answer 1: %d"

exampleInput |> allTheBeams |> printfn "Example answer 2: %d"
input |> allTheBeams |> printfn "Answer 2: %d"