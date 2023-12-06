// Macro to get running time
#time 

type Seed = {
    id: int64
    range: int64
}

type Converter = {
    destination: int64
    source: int64
    range: int64
}

// Newline
let nl = "\n"

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl+nl, System.StringSplitOptions.RemoveEmptyEntries)

let stringToInt64Array (s: string) =
    s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int64

let getMaps (sArr: string array) = 
    sArr
    |> Array.skip 1
    |> Array.map (fun s -> 
        s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.skip 1 // Name of the map
        |> Array.map stringToInt64Array
        |> Array.map (fun arr -> {
            destination = arr.[0]
            source = arr.[1]
            range = arr.[2]}))

let inputToSeedLocations getSeeds (sArr: string array) = 
    let seeds : Seed array = getSeeds sArr.[0]
    let maps = getMaps sArr
    seeds 
    |> Array.map (fun seed -> 
        [|seed.id .. seed.id + seed.range - 1L|])
    |> Array.concat
    |> Array.map (fun iSeed -> 
        [|0..maps.Length - 1|]
        |> Array.fold (fun state i -> 
            let conv = maps.[i] |> Array.tryFind (fun c -> 
                state >= c.source 
                && state <= c.source + c.range - 1L )
            match conv with
            | None -> state 
            | _ -> 
                let c = conv.Value
                let diff = state - c.source
                c.destination + diff
            ) iSeed)

let getSeedsOneByOne (s : string) = 
    s.Split(": ").[1] 
    |> stringToInt64Array 
    |> Array.map (fun i -> {
        id = i
        range = 1
    })
let getSeedRange (s: string) = 
    s.Split(": ").[1] 
    |> stringToInt64Array
    |> Array.chunkBySize 2
    |> Array.collect Array.pairwise
    |> Array.map (fun (i, r) -> {
        id = i
        range = r})

let exampleInput = parseInput "./input/day05_example.txt"
let input = parseInput "./input/day05.txt" 

exampleInput
|> inputToSeedLocations getSeedsOneByOne
|> Array.min
|> printfn "Example answer 1: %d"

input 
|> inputToSeedLocations getSeedsOneByOne
|> Array.min
|> printfn "Answer 1: %d"

// It kind of works, but explodes to too big tables in part 2
exampleInput
|> inputToSeedLocations getSeedRange
|> Array.min
|> printfn "Example answer 2: %d"

type largeInt = int64
exception BruteForceFinishedError of string

let part2InverterMadnessBruteForce (sArr: string array) = 
    let maps = sArr |> getMaps
    let seedRanges = sArr.[0] |> getSeedRange
    for i in 0L .. largeInt.MaxValue do
        let potentialSeed = 
            maps 
            |> Array.rev
            |> Array.fold (fun (state: int64) (mapArr: Converter array) ->
                let conv = 
                    mapArr 
                    |> Array.tryFind (fun c -> 
                        state >= c.destination 
                        && state <= c.destination + c.range - 1L )
                match conv with
                | None -> state 
                | _ -> 
                    let c = conv.Value
                    let diff = state - c.destination
                    c.source + diff
                ) i 
        let seedConv = 
            seedRanges
            |> Array.tryFind (fun c -> 
                potentialSeed >= c.id 
                && potentialSeed <= c.id + c.range - 1L )
        if (seedConv.IsSome) 
        then 
            printfn "Answer 2: %A" i
            raise (BruteForceFinishedError "this was stupid") 
     
input
|> part2InverterMadnessBruteForce