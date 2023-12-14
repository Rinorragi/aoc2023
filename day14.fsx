// Macro to get running time
#time 

open System

let nl = "\n"
type RockType = 
    | Solid
    | Rolling
type RollDirection =
    | North
    | East
    | South
    | West
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
                | '#' -> Some(new Rock(Guid.NewGuid(), RockType.Solid, x, y))
                | 'O' -> Some(new Rock(Guid.NewGuid(),RockType.Rolling, x, y))
                | _ -> None)
             |> List.choose id)
        |> List.concat

let rocksMaxY (rocks: Rock list) = (rocks |> List.maxBy (fun r -> r.y)).y

let rollNorth (rocks: Rock list) =
    let maxY = rocksMaxY rocks
    [0..maxY] 
    |> List.fold (fun (state: Rock list) (y: int) -> 
        let movedRocks = 
            state |> List.filter(fun r -> r.y = y && r.rockType = RockType.Rolling)
            |> List.map (fun r -> 
                new Rock(r.id, r.rockType, r.x, 
                    state 
                    |> List.filter (fun r -> r.y < y) 
                    |> List.filter(fun rx -> rx.x = r.x) 
                    |> fun rxs -> 
                        if rxs.Length > 0 
                        then (rxs |> List.maxBy (fun r -> r.y)).y + 1 
                        else 0))
        let movedRockIds = movedRocks |> List.map (fun r -> r.id)
        let unmovedRocks = 
            state 
            |> List.filter (fun r -> not (movedRockIds |> List.contains r.id))
        unmovedRocks @ movedRocks
    ) rocks

let calculateTotalLoad (rocks: Rock list) =
    let maxY = rocksMaxY rocks + 1
    rocks 
    |> List.filter(fun r -> r.rockType = RockType.Rolling)
    |> List.map(fun r -> maxY - r.y)
    |> List.sum


let exampleRocks = parseInput "./input/day14_example.txt" 
let rocks = parseInput "./input/day14.txt" 
exampleRocks |> rollNorth |> calculateTotalLoad |> printfn "Example answer 1: %d"
rocks |> rollNorth |> calculateTotalLoad |> printfn "Answer 1: %d"
