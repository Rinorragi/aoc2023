// Macro to get running time
#time 

type Direction =
    | North
    | West
    | South
    | East

type Instruction = {
    dir: Direction
    amount: int
}

type Coordinate = {
    x: int
    y: int
}

let parsePart1 pathString =
    System.IO.File.ReadAllLines pathString
    |> Array.map (fun s -> 
        let sArr = s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        {
            dir = 
                match sArr.[0] with 
                | "R" -> Direction.East
                | "D" -> Direction.South 
                | "L" -> Direction.West  
                | "U" -> Direction.North 
                | _ -> failwith (sprintf "Wrong string: %s" sArr.[0])
            amount = sArr.[1] |> int
        })
    |> List.ofArray

let parsePart2 pathString =
    System.IO.File.ReadAllLines pathString
    |> Array.map (fun s -> 
        let sArr = s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        let hexaString = sArr.[2].Substring(2,5)
        let ins = sArr.[2].Substring(sArr.[2].Length - 2,1)
        {
            dir = 
                match ins with 
                | "0" -> Direction.East
                | "1" -> Direction.South 
                | "2" -> Direction.West  
                | "3" -> Direction.North 
                | _ -> failwith (sprintf "Wrong string: %s" ins)
            amount = System.Convert.ToInt32(hexaString,16)
        })
    |> List.ofArray

let instructionsToCoordinates (instructions: Instruction list) =
    let coordinates = 
        instructions
        |> List.fold (fun state ins -> 
            let from = state |> List.last
            let newCoord = 
                match ins.dir with 
                | North -> { x=from.x; y=from.y - ins.amount;}
                | South -> { x=from.x; y = from.y + ins.amount;}
                | West -> { x=from.x - ins.amount; y = from.y;}
                | East -> { x=from.x + ins.amount; y = from.y;}
            state @ [newCoord]
        ) [{x=0; y=0;}]
    
    let yMin = coordinates |> List.minBy (fun c -> c.y) |> (fun c -> c.y)
    let xMin = coordinates |> List.minBy (fun c -> c.x) |> (fun c -> c.x)

    let coordinates = 
        coordinates 
        |> List.map (fun c -> 
            let newY = if yMin < 0 then c.y + abs(yMin) else c.y
            let newX = if xMin < 0 then c.x + abs(xMin) else c.x
            { x=newX; y=newY;})
    let insructionsAmount = 
        System.Math.Round(
            // + 1 from the starting point
            (((instructions |> List.sumBy (fun f -> f.amount |> float)) + 1.0) / 2.0),
            System.MidpointRounding.AwayFromZero)
    (coordinates, insructionsAmount)
    
let calculateArea (coordinates: Coordinate list, len: float) =
    // Shoelace formula for area of polygon. Nigel Galloway: April 11th., 2018
    let shoeLace(n::g) = abs(List.pairwise(n::g@[n])|>List.fold(fun n ((nα,gα),(nβ,gβ))->n+(nα*gβ)-(gα*nβ)) 0.0)/2.0
    //printfn "%f" (shoeLace [(3.0,4.0); (5.0,11.0); (12.0,8.0); (9.0,5.0); (5.0,6.0)])
    let floatAnswer = 
        coordinates 
        |> List.map (fun f -> (f.x |> float, f.y |> float))
        |> shoeLace 
        |> (+) len
    System.Math.Round(floatAnswer,System.MidpointRounding.AwayFromZero)

parsePart1 "./input/day18_example.txt" |> instructionsToCoordinates |> calculateArea |> printfn "Example answer 1: %f" 
parsePart2 "./input/day18_example.txt" |> instructionsToCoordinates |> calculateArea |> printfn "Example answer 2: %f" 
parsePart1 "./input/day18.txt" |> instructionsToCoordinates |> calculateArea |> printfn "Answer 1: %f" 
parsePart2 "./input/day18.txt" |> instructionsToCoordinates |> calculateArea |> printfn "Answer 2: %f" 