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
    color: string
}

type PixelType = 
    | Empty
    | Filled
    | Digged

type Coordinate = {
    x: int
    y: int
    t: PixelType
    color: string
}

let white = "#000000"

let parse pathString =
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
            color = sArr.[2].Substring(1,sArr.[2].Length - 2)
        })
    |> List.ofArray


let instructionsToCoordinates (instructions: Instruction list) =
    let coordinates = 
        instructions
        |> List.fold (fun state ins -> 
            let from = state |> List.last
            let newCoordinates = 
                match ins.dir with 
                | North ->
                    [1..ins.amount] |> List.map (fun i -> { x=from.x; y=from.y - i; t=PixelType.Digged; color=ins.color})
                | South ->
                    [1..ins.amount] |> List.map (fun i -> { x=from.x; y = from.y + i; t=PixelType.Digged; color = ins.color})
                | West ->
                    [1..ins.amount] |> List.map (fun i -> { x=from.x - i; y = from.y; t=PixelType.Digged; color = ins.color})
                | East -> 
                    [1..ins.amount] |> List.map (fun i -> { x=from.x + i; y = from.y; t=PixelType.Digged; color = ins.color})
            state @ newCoordinates
        ) [{x=0; y=0; t=PixelType.Digged; color=white}]
    
    let yMin = coordinates |> List.minBy (fun c -> c.y) |> (fun c -> c.y)
    let xMin = coordinates |> List.minBy (fun c -> c.x) |> (fun c -> c.x)

    coordinates 
    |> List.map (fun c -> 
        let newY = if yMin < 0 then c.y + abs(yMin) else c.y
        let newX = if xMin < 0 then c.x + abs(xMin) else c.x
        { x=newX; y=newY; t=c.t; color=c.color})

let instructionsToGrid (instructions: Instruction list) =
    let coordinates = instructionsToCoordinates instructions
    let coordinateMap = coordinates |> List.map (fun c -> (c.x, c.y),c) |> Map.ofList
    let yMax = coordinates |> List.maxBy (fun c -> c.y) |> (fun c -> c.y)
    let xMax = coordinates |> List.maxBy (fun c -> c.x) |> (fun c -> c.x)
    [0..yMax]
    |> List.map (fun newY -> 
        [0..xMax]
        |> List.fold (fun (pixelArr) newX ->  
            let pixel = 
                match coordinateMap.ContainsKey (newX,newY) with
                | false -> {x=newX;y=newY;t=PixelType.Empty;color=white}
                | true -> coordinateMap.[(newX,newY)]
            (pixelArr @ [pixel])
            ) ([]))

let printGrid (grid: Coordinate list list) =
    grid
    |> List.map (fun cList -> 
        cList 
        |> List.map (fun c -> 
            match c.t with 
            | PixelType.Digged -> '#'
            | PixelType.Filled -> '?'
            | PixelType.Empty -> '_')
        |> Array.ofList)
    |> List.map System.String
    |> List.map (fun s -> printfn "%s" s)

let solvePart1 (grid: Coordinate list) =
    let len = (grid.Length |> float) / 2.0
    // Shoelace formula for area of polygon. Nigel Galloway: April 11th., 2018
    let shoeLace(n::g) = abs(List.pairwise(n::g@[n])|>List.fold(fun n ((nα,gα),(nβ,gβ))->n+(nα*gβ)-(gα*nβ)) 0.0)/2.0
    //printfn "%f" (shoeLace [(3.0,4.0); (5.0,11.0); (12.0,8.0); (9.0,5.0); (5.0,6.0)])
    let floatAnswer = 
        grid 
        |> List.filter (fun c -> c.t = PixelType.Digged)
        |> List.map (fun f -> (f.x |> float, f.y |> float))
        |> shoeLace 
        |> (+) len
    System.Math.Round(floatAnswer,System.MidpointRounding.AwayFromZero)
//    |> List.length

let exampleInput = parse "./input/day18_example.txt"
//let exampleGrid  = exampleInput |> instructionsToGrid 
//exampleGrid |> printGrid
exampleInput |> instructionsToCoordinates |> solvePart1 |> printfn "Example answer 1: %A" 

let input = parse "./input/day18.txt"
//let grid = input |> instructionsToGrid 
input |> instructionsToCoordinates |> solvePart1 |> printfn "Example answer 1: %A" 
//grid |> printGrid
//grid |> solvePart1 |> printfn "Answer 1: %A" 
//input |> printfn "%A"