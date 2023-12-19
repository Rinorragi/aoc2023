#time

// End of copy paste
type Coordinate = {
    id: string
    x: int
    y: int
    heatloss: int
}

//Dijkstra's algorithm: Nigel Galloway, August 5th., 2018
//https://rosettacode.org/wiki/Dijkstra%27s_algorithm#F#
[<CustomEquality;CustomComparison>]
type Dijkstra<'N,'G when 'G:comparison>=
    {toN:'N;cost:Option<'G>;fromN:'N}
    override g.Equals n =
        match n with
        | :? Dijkstra<'N,'G> as n->n.cost=g.cost
        |_->false
    override g.GetHashCode() = hash g.cost
    interface System.IComparable with
        member n.CompareTo g =
            match g with
            | :? Dijkstra<'N,'G> as n when n.cost=None -> (-1)
            | :? Dijkstra<'N,'G> when n.cost=None -> 1
            | :? Dijkstra<'N,'G> as g -> compare n.cost g.cost
            | _-> invalidArg "n" "expecting type Dijkstra<'N,'G>"

let Dijkstra (coordinatePoints: Coordinate list) (graph: Map<(Coordinate * Coordinate), int>) (start: Coordinate) =
  let rec findPath (nextSteps: Dijkstra<Coordinate, int> list) (seen: (Coordinate * Coordinate) list) =
    if List.isEmpty nextSteps 
    then seen
    else
        let costEfficientNext = List.min nextSteps
        printfn "%A" costEfficientNext.cost
        if costEfficientNext.cost=None 
        then seen 
        else
            let recNextSteps = 
                nextSteps 
                |> List.choose(fun potentialNextStep ->
                    if potentialNextStep.toN=costEfficientNext.toN 
                    then None 
                    else 
                        match costEfficientNext.cost, potentialNextStep.cost, Map.tryFind (costEfficientNext.toN,potentialNextStep.toN) graph with
                            |Some g,None,Some wg->Some {toN=potentialNextStep.toN;cost=Some(g+wg);fromN=costEfficientNext.toN}
                            |Some g,Some g',Some wg when g+wg<g'->Some {toN=potentialNextStep.toN;cost=Some(g+wg);fromN=costEfficientNext.toN}
                            |_ ->Some potentialNextStep)
            let appendedSeen = ((costEfficientNext.fromN,costEfficientNext.toN)::seen) 
            findPath recNextSteps appendedSeen
  let route = findPath (coordinatePoints |>List.map(fun n->{toN=n;cost=(Map.tryFind(start,n) graph);fromN=start})) []
  (fun endCoordinate ->
    let rec buildRoute (endCoord: Coordinate) (linearRoute: Coordinate list) =
        match List.tryFind(fun (_,g)->g=endCoord) route with
        |Some(startC, endC) when start=startC->Some(startC::endC::linearRoute)
        |Some(startC, endC) ->buildRoute startC (endC::linearRoute)
        |_ ->None
    buildRoute endCoordinate [])

let mazeInput (pathString: string) =     
    System.IO.File.ReadAllLines pathString
    |> Array.map Array.ofSeq
    |> Array.mapi (fun iy row ->
        row |> Array.mapi (fun ix c -> {
                id = sprintf "%d_%d" ix iy
                x = ix
                y = iy
                heatloss = int c - int '0'
            }))

let fetchCoordinate (maze: Coordinate array array) xy =
    if fst xy < 0 
        || fst xy > maze.[0].Length - 1 
        || snd xy < 0 
        || snd xy > maze.Length - 1
    then 
        None
    else 
        Some(maze.[snd xy].[fst xy])

let mazeGraph (maze: Coordinate array array) = 
    maze
    |> Array.mapi (fun y row ->
        row |> Array.mapi (fun x xy -> 
            let up = fetchCoordinate maze (x, y + 1) 
            let down = fetchCoordinate maze (x, y - 1)
            let right = fetchCoordinate maze (x + 1, y)
            let left = fetchCoordinate maze (x - 1, y)
            [(xy,up);(xy,down);(xy,right);(xy,left)] 
            |> List.filter (snd >> Option.isSome) 
            |> List.map (fun (xy,other) -> (xy, other.Value)))
        |> List.ofArray
        |> List.concat)
    |> List.ofArray
    |> List.concat
    |> List.map (fun (c1: Coordinate, c2: Coordinate) -> ((c1,c2),c2.heatloss))
    |> Map.ofList

let part1Solver (pathString: string) = 
    let input =  mazeInput pathString
    let mazePoints = input |> Array.concat |> List.ofArray
    let mGraph = mazeGraph input
    let start = mazePoints |> List.find (fun f -> f.x = 0 && f.y = 0)
    let goal = mazePoints |> List.find (fun f -> f.x = (mazePoints |> List.maxBy (fun xy -> xy.x)).x && f.y = (mazePoints |> List.maxBy (fun xy -> xy.y)).y )
    let path = Dijkstra mazePoints mGraph start goal
    if path.IsSome 
    then
        let l = path.Value
        printfn "%A" l
        l |> List.skip 1 |> List.sumBy (fun f -> f.heatloss)
    else 
        -1 
    
part1Solver "./input/day17_example.txt" |> printfn "Example answer1: %A"
