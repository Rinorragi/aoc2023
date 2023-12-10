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
    | 'S' -> [None]
    | _ -> invalidArg "c" (sprintf "Invalid character: %c" c)

let directionToIndex dir rowLength index  = 
    match dir with 
    | North -> index - rowLength
    | South -> index + rowLength
    | West -> index - 1
    | East -> index + 1
    | None -> index

let startDirection (input: string) (rowLength: int) (index: int) =
    if possibleDirection input.[index-rowLength] |> List.contains(South) then (North, directionToIndex North rowLength index)
    elif possibleDirection input.[index+rowLength] |> List.contains(North) then (South, index + rowLength)
    elif possibleDirection input.[index - 1] |> List.contains(East) then (West, index - 1)
    else (East, index + 1)

let getOppositeLastDirection dir = 
    match dir with 
    | North -> South
    | South -> North
    | West -> East 
    | East -> West
    | None -> None

let rec traverseLoop (input: string) (rowLength: int) (lastDir: Direction) (currentIndex: int) (loopLocations: int list) =
    if loopLocations |> List.contains currentIndex 
        || lastDir = None 
    then loopLocations
    else
        let oppositeDir = getOppositeLastDirection lastDir
        let newDir = 
            possibleDirection input.[currentIndex] 
            |> List.filter (fun x -> x <> oppositeDir) // Prevent going back
            |> List.head
        let newIndex = directionToIndex newDir rowLength currentIndex
        traverseLoop input rowLength newDir newIndex (List.append loopLocations [currentIndex])

let traverseSetup (filePath: string) = 
    let input = parseInput filePath
    let rowLength = input.IndexOf(nl) + 1
    let animalLocation = input.IndexOf(animal)
    let startDir = startDirection input rowLength animalLocation
    traverseLoop input rowLength (fst startDir) (snd startDir) [animalLocation]

let solvePart1 (route: int list) =
    route.Length / 2 + route.Length % 2

traverseSetup "./input/day10_example.txt" |> solvePart1 |> printfn "Example answer 1: %A"
traverseSetup "./input/day10.txt" |> solvePart1 |> printfn "Answer 1: %A"
