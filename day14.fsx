// Macro to get running time
#time 

open System

let nl = "\n"
type RockType = 
    | Unmovable
    | Rolling
type RollDirection =
    | North
    | West
    | South
    | East
type Rock =
    struct 
        val id: Guid
        val rockType: RockType
        val x: int
        val y: int
        new(gid, t, rx, ry) = {id = gid; rockType = t; x = rx; y = ry }
    end

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> 
        s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray 
        |> List.mapi (fun y s -> 
            s.ToCharArray() 
            |> List.ofArray 
            |> List.mapi (fun x c ->
                match c with 
                | '#' -> Some(new Rock(Guid.NewGuid(), RockType.Unmovable, x, y))
                | 'O' -> Some(new Rock(Guid.NewGuid(),RockType.Rolling, x, y))
                | _ -> None)
             |> List.choose id)
        |> List.concat

let rocksMaxY (rocks: Rock list) = (rocks |> List.maxBy (fun r -> r.y)).y
let rocksMaxX (rocks: Rock list) = (rocks |> List.maxBy (fun r -> r.x)).x

let updateRockState (movedRocks: Rock list) (rocks: Rock list) =
    let movedRockIds = movedRocks |> List.map (fun r -> r.id)
    let unmovedRocks = 
        rocks 
        |> List.filter (fun r -> not (movedRockIds |> List.contains r.id))
    unmovedRocks @ movedRocks

let rollSouthNorth (yList: int list) blockFilterFunc newYFunc freeY (unmovaBle: Rock list) (rocks: Rock list) = 
    yList
    |> List.fold (fun (state: Rock list) (y: int) -> 
        let movedRocks = 
            state 
            |> List.filter(fun r -> r.y = y)
            |> List.map (fun r -> 
                let newY = 
                    (state @ unmovaBle) 
                    |> List.filter(fun r -> blockFilterFunc r y)
                    |> List.filter(fun rx -> rx.x = r.x) 
                    |> fun rxs -> 
                        if rxs.Length > 0 
                        then newYFunc rxs // (rxs |> List.maxBy (fun r -> r.y)).y + 1 
                        else freeY
                new Rock(r.id, r.rockType, r.x, newY))
        updateRockState movedRocks state
    ) rocks

let rollWestEast (xList: int list) blockFilterFunc newXFunc freeX (unmovaBle: Rock list) (rocks: Rock list) = 
    xList
    |> List.fold (fun (state: Rock list) (x: int) -> 
        let movedRocks = 
            state 
            |> List.filter(fun r -> r.x = x)
            |> List.map (fun r -> 
                let newX = 
                    (state @ unmovaBle) 
                    |> List.filter(fun r -> blockFilterFunc r x)
                    |> List.filter(fun rx -> rx.y = r.y) 
                    |> fun rxs -> 
                        if rxs.Length > 0 
                        then newXFunc rxs
                        else freeX
                new Rock(r.id, r.rockType, newX, r.y))
        updateRockState movedRocks state
    ) rocks

let roll (dir: RollDirection) (maxY: int) (maxX: int) (unmovaBle: Rock list) (rocks: Rock list) =
    match dir with 
    | RollDirection.North ->
        rollSouthNorth 
            [0..maxY] 
            (fun (r: Rock) (y: int) ->  r.y < y) 
            (fun (rxs: Rock list) -> (rxs |> List.maxBy (fun r -> r.y)).y + 1)
            0
            unmovaBle
            rocks
    | RollDirection.South ->
        rollSouthNorth
            ([0..maxY] |> List.rev)
            (fun (r: Rock) (y: int) ->  r.y > y) 
            (fun (rxs: Rock list) -> (rxs |> List.minBy (fun r -> r.y)).y - 1)
            maxY
            unmovaBle
            rocks
    | RollDirection.West ->
        rollWestEast 
            [0..maxX] 
            (fun (r: Rock) (x: int) ->  r.x < x) 
            (fun (rxs: Rock list) -> (rxs |> List.maxBy (fun r -> r.x)).x + 1)
            0
            unmovaBle
            rocks
    | RollDirection.East ->
        rollWestEast 
            ([0..maxX] |> List.rev)
            (fun (r: Rock) (x: int) ->  r.x > x) 
            (fun (rxs: Rock list) -> (rxs |> List.minBy (fun r -> r.x)).x - 1)
            maxX
            unmovaBle
            rocks

let calculateTotalLoad (rocks: Rock list) =
    let maxY = rocksMaxY rocks + 1
    rocks 
    |> List.filter(fun r -> r.rockType = RockType.Rolling)
    |> List.map(fun r -> maxY - r.y)
    |> List.sum

let rollCycleTimes (nTotal: int) (rocks: Rock list) = 
    let maxY = rocksMaxY rocks
    let maxX = rocksMaxX rocks
    let rollingRocks = rocks |> List.filter (fun x -> x.rockType = RockType.Rolling)
    let unmovableRocks = rocks |> List.filter (fun x -> x.rockType = RockType.Unmovable)

    let rec findLoadWindow (nCounter: int) (localMax: int) (loads: int list) = 
        if nCounter > localMax then -1
        else 
            let evenNumber = loads.Length / nCounter
            let windowedloads = 
                loads
                |> List.take (evenNumber * nCounter) 
                |> List.chunkBySize nCounter
            if windowedloads |> List.forall (fun win -> win = windowedloads.Head) 
            then nCounter
            else findLoadWindow (nCounter + 1) localMax loads

    let rec rollCycle (nCounter: int) (maxY: int) (maxX: int) (recRocks: Rock list) (loads: int list) =  
        let windowMax = loads.Length / 10
        let nSkip = loads.Length / 2 // Enough rounds to make situation settle down
        let skipWindow = loads |> List.skip nSkip
        let windowSize = findLoadWindow 2 windowMax skipWindow
        if (nCounter % 100 = 0) then printfn "Rolling cycle: %d/%d skip %d windowMax %d" nCounter nTotal nSkip windowMax
        if (windowSize <> -1) 
        then 
            let modus = (nTotal - nSkip) % windowSize
            let result = skipWindow.[modus]
            result
        else 
            let newRocks = 
                recRocks 
                |> roll RollDirection.North maxY maxX unmovableRocks
                |> roll RollDirection.West maxY maxX unmovableRocks
                |> roll RollDirection.South maxY maxX unmovableRocks
                |> roll RollDirection.East maxY maxX unmovableRocks
            let newTotalLoad = recRocks |> calculateTotalLoad
            rollCycle (nCounter + 1) maxY maxX newRocks (loads @ [newTotalLoad])
    rollCycle 0 maxY maxX rollingRocks []

let rollNorthOnce (rocks: Rock list) = 
    let maxY = rocksMaxY rocks
    let maxX = rocksMaxX rocks
    let rollingRocks = rocks |> List.filter (fun x -> x.rockType = RockType.Rolling)
    let unmovableRocks = rocks |> List.filter (fun x -> x.rockType = RockType.Unmovable)
    rocks |> roll RollDirection.North maxY maxX unmovableRocks

let exampleRocks = parseInput "./input/day14_example.txt" 
let rocks = parseInput "./input/day14.txt" 
// Part 1
exampleRocks |> rollNorthOnce |> calculateTotalLoad |> printfn "Example answer 1: %d"
rocks |> rollNorthOnce |> calculateTotalLoad |> printfn "Answer 1: %d"

// Part 2
exampleRocks |> rollCycleTimes 1000000000 |> printfn "Example answer 2: %d"
rocks |> rollCycleTimes 1000000000 |> printfn "Answer 2: %d"
