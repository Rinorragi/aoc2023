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

parseInput "./input/day05_example.txt" 
|> inputToSeedLocations getSeedsOneByOne
|> Array.min
|> printfn "Example answer 1: %d"


parseInput "./input/day05.txt" 
|> inputToSeedLocations getSeedsOneByOne
|> Array.min
|> printfn "Answer 1: %d"

parseInput "./input/day05_example.txt" 
|> inputToSeedLocations getSeedRange
|> Array.min
|> printfn "Example answer 2: %d"

//FIX: Arithmetic operation overflow in arrays
//parseInput "./input/day05.txt" 
//|> inputToSeedLocations getSeedRange
//|> Array.min
//|> printfn "Answer 2: %d"