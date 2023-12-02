// Macro to get running time
#time 

// Newline
let nl = "\n"

type Game = {
    id: int
    red: int64 array
    green: int64 array
    blue: int64 array
    power: int64
}

let parseInput (filePath) stringFilter =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(fun s -> 
        let gameId = s.Split(':').[0].Substring("Game ".Length - 1) |> int
        let coloredCubes = 
            s.Split(": ").[1].Split(',', ';') 
            |> Array.map (fun scc -> 
                let amount = scc.Trim().Split(" ").[0] |> int64
                let color = scc.Trim().Split(" ").[1]
                (color, amount))

        let red = coloredCubes |> Array.filter (fun cc -> fst cc = "red") |> Array.map snd
        let green = coloredCubes |> Array.filter (fun cc -> fst cc = "green") |> Array.map snd
        let blue = coloredCubes |> Array.filter (fun cc -> fst cc = "blue") |> Array.map snd
        
        {
            id = gameId
            red = red
            green = green
            blue = blue
            power = [|red; green; blue|] |> Array.map Array.max |> Array.fold (*) 1
        }
    )

let red12green13blue14Filter (game : Game) = 
    (Array.max game.red <= 12) 
    && (Array.max game.green <= 13) 
    && (Array.max game.blue <= 14)

let exampleGames = parseInput "./input/day02_example.txt" (fun (s : string) -> s)
let games = parseInput "./input/day02.txt" (fun (s : string) -> s)

// Part 1
exampleGames
|> Array.filter red12green13blue14Filter
|> Array.sumBy (fun g -> g.id)
|> printfn "Example answer part 1: %A"
|> ignore

games
|> Array.filter red12green13blue14Filter
|> Array.sumBy (fun g -> g.id)
|> printfn "Answer 1: %A"
|> ignore

// Part 2
exampleGames
|> Array.sumBy(fun g -> g.power)
|> printfn "Example answer part 2: %A"
|> ignore


games
|> Array.sumBy(fun g -> g.power)
|> printfn "Answer 2: %A"
|> ignore