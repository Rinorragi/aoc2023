// Macro to get running time
#time 

type Direction = 
    | North
    | South
    | West
    | East
    | None

// Newline
let nl = "\n"
let animal = 'S'

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)

let possibleDirection (c: char) = 
    match c with 
    | '|' -> [North; South]
    | '-' -> [East; West]
    | 'L' -> [North; East]
    | 'J' -> [North; West]
    | '7' -> [South; West]
    | 'F' -> [South; East]
    | '.' 
    | 'S'
    | _ -> [None]

let directionToIndex dir rowLength index  = 
    match dir with 
    | North -> index - rowLength
    | South -> index + rowLength
    | West -> index - 1
    | East -> index + 1
    | None -> index

let startDirection (input: string) (rowLength: int) (index: int) =
    if index - rowLength >= 0 && possibleDirection input.[index-rowLength] |> List.contains(South) then (North, directionToIndex North rowLength index)
    elif index + rowLength + 1 < input.Length && possibleDirection input.[index+rowLength] |> List.contains(North) then (South, index + rowLength)
    elif index - 1 >= 0 && possibleDirection input.[index - 1] |> List.contains(East) then (West, index - 1)
    else (East, index + 1)

let getOppositeLastDirection dir = 
    match dir with 
    | North -> South
    | South -> North
    | West -> East 
    | East -> West
    | None -> None

let rec traverseLoop (input: string) (rowLength: int) (lastDir: Direction) (currentIndex: int) (loopLocations : (Direction * int) list) =
    if loopLocations |> List.exists (fun f -> snd f = currentIndex) 
        || lastDir = None 
    then loopLocations
    else
        let oppositeDir = getOppositeLastDirection lastDir
        let newDir = 
            possibleDirection input.[currentIndex] 
            |> List.filter (fun x -> x <> oppositeDir) // Prevent going back
            |> List.head
        let newIndex = directionToIndex newDir rowLength currentIndex
        traverseLoop input rowLength newDir newIndex (List.append loopLocations [(newDir, currentIndex)])

let traverseSetup (input: string) (rowLength: int) = 
    let animalLocation = input.IndexOf(animal)
    let startDir = startDirection input rowLength animalLocation
    traverseLoop input rowLength (fst startDir) (snd startDir) [(fst startDir, animalLocation)]

let solvePart1 (pipe: (Direction * int) list) =
    pipe.Length / 2 + pipe.Length % 2

let rec getPartsToDirection (input: string) (rowLength: int) (pipe: (Direction * int) list) (dir: Direction) (index: int) (indexes: int list) =
    let newIndex = directionToIndex dir rowLength index
    if (index < 0 || index >= input.Length) then failwith "Wrong direction"
    if (pipe |> List.exists (fun f -> snd f = newIndex)) 
    then indexes
    else 
        getPartsToDirection input rowLength pipe dir newIndex (List.append indexes [newIndex]) 

let clockWise direction =
    match direction with
    | North -> East
    | East -> South
    | South -> West
    | West -> North
    | None -> failwith "pipePart was none"

let counterClockWise direction = 
    match direction with
    | North -> West
    | West -> South
    | South -> East
    | East -> North
    | None -> failwith "pipePart was none"

let solveNestArea directionFunction (input: string) (rowLength: int) (pipe: (Direction * int) list) =
    pipe 
    |> List.fold (fun (state: int list) pipePart -> 
        let dir90 = directionFunction (fst pipePart)
        let newState = getPartsToDirection input rowLength pipe dir90 (snd pipePart) state
        newState) ([])
    |> List.distinct

let findNestArea (input: string) (rowLength: int) (pipe: (Direction * int) list) =
    try 
        solveNestArea counterClockWise input rowLength pipe
    with 
        | _ -> solveNestArea clockWise input rowLength pipe

let exampleInput1 = parseInput "./input/day10_example.txt" 
let exampleRowLength1 = exampleInput1.IndexOf(nl) + 1
let examplePipe1 = traverseSetup exampleInput1 exampleRowLength1
examplePipe1 |> solvePart1 |> printfn "Example answer 1: %A"

let input = parseInput "./input/day10.txt" 
let rowLength = input.IndexOf(nl) + 1
let pipe = traverseSetup input rowLength
pipe |> solvePart1 |> printfn "Answer 1: %A"

let exampleInput2 = parseInput "./input/day10_example2.txt" 
let exampleRowLength2 = exampleInput2.IndexOf(nl) + 1
let examplePipe2 = traverseSetup exampleInput2 exampleRowLength2
findNestArea exampleInput2 exampleRowLength2 examplePipe2 |> List.length |> printfn "Example Answer 2: %A"


let exampleInput3 = parseInput "./input/day10_example3.txt" 
let exampleRowLength3 = exampleInput3.IndexOf(nl) + 1
let examplePipe3 = traverseSetup exampleInput3 exampleRowLength3
findNestArea exampleInput3 exampleRowLength3 examplePipe3 |> List.length |> printfn "Example answer 2.1: %A"

findNestArea input rowLength pipe |> List.length |> printfn "Answer 2: %d"