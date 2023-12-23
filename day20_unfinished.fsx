// Macro to get running time
#time 

let nl = "\n"

type SignalType = 
    | Low
    | High
    | None

type Module = 
    | FlipFlop of mid: string * outputs: string list * onOff: bool 
    | Broadcast of mid: string * outputs: string list
    | Conjuction of mid: string * inputs: (string * SignalType) list * outputs: string list
    | Untyped of mid: string

let moduleId (m: Module) = 
    match m with 
    | FlipFlop (mid = sid)
    | Broadcast (mid = sid)
    | Untyped (mid = sid)
    | Conjuction (mid = sid) -> sid

let parseInput (filePath) =
    let modlist = 
        System.IO.File.ReadAllLines filePath
        |> List.ofArray
        |> List.map(fun s ->
            let sSplit = s.Split(" -> ", System.StringSplitOptions.RemoveEmptyEntries)
            let connectedModules = sSplit.[1].Split(", ", System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            match sSplit.[0].[0], sSplit.[0] with 
            | '%',_ -> 
                let sid = sSplit.[0].Substring(1)
                (sid, FlipFlop(mid = sid, outputs = connectedModules, onOff = false))
            | '&',_ -> 
                let sid = sSplit.[0].Substring(1)
                (sid, Conjuction(mid = sid, inputs = [], outputs = connectedModules))
            | 'b',"broadcaster" ->
                let sid = sSplit.[0]
                (sid, Broadcast(mid = sid, outputs = connectedModules))
            | _ -> failwith (sprintf "Weird string: %s" s))
    modlist
    |> List.map(fun (sid,m) -> 
        let updatedModule = 
            match m with 
            | Conjuction(outputs = o) -> 
                let sInputs = 
                    modlist 
                    |> List.filter (fun (inputId,m) -> 
                        let oos = 
                            match m with 
                            | FlipFlop(outputs = out) -> out
                            | Broadcast(outputs = out) -> out
                            | Conjuction(outputs = out) -> out
                            | _ -> []
                        oos |> List.contains sid)
                    |> List.map (fun (inputId,_) -> (inputId, SignalType.Low))
                Conjuction(mid = sid, inputs = sInputs, outputs = o)
            | _ -> m
        (sid,updatedModule))
    |> Map.ofList
    

let rec HandlePulses (modules: Map<string,Module>) (signals: (string * string * SignalType) list) (lowCount:int64, highCount:int64) =
    if (signals |> List.isEmpty) 
    then 
        (modules, lowCount, highCount)
    else 
        // Update module states and get new signals
        let updateList = 
            signals 
            |> List.map (fun (inputId, outputId, signal) -> 
                let updatedModule = 
                    if (not(modules.ContainsKey outputId)) then
                        Untyped(mid = outputId)
                    else 
                        let currentModule = modules.[outputId]
                        // Update module statuses
                        let updModule = 
                            match currentModule with
                            | FlipFlop (outputs = o; onOff = currentState) ->
                                FlipFlop(mid = outputId, outputs = o, onOff = if signal = SignalType.Low then not(currentState) else currentState)
                            | Conjuction (inputs = ins; outputs = o) -> 
                                let updatedInputs = 
                                    ins 
                                    |> List.filter (fun (sid: string, sType: SignalType) -> sid <> inputId)
                                Conjuction(mid = outputId, inputs = (updatedInputs @ [(inputId, signal)]), outputs = o)
                            | Broadcast (outputs = o) -> currentModule
                            | Untyped (mid = outputId) -> currentModule
                        updModule
                // Figure out new signals to send
                let newSignals = 
                    match updatedModule with 
                    | Untyped (mid = outputId) -> []
                    | FlipFlop (outputs = o)
                    | Broadcast (outputs = o)
                    | Conjuction (outputs = o) ->
                        let newSignalType = 
                            match updatedModule with 
                            | Untyped (mid = outputId) -> SignalType.None
                            | FlipFlop (onOff = newOnOff) -> if signal = SignalType.High then None elif newOnOff then SignalType.High else SignalType.Low
                            | Broadcast (mid = bid) -> signal
                            | Conjuction (inputs = i) ->
                                if (i |> List.map snd |> List.forall (fun s -> s = SignalType.High)) then SignalType.Low
                                else SignalType.High
                        o |> List.map (fun oid -> (outputId, oid, newSignalType))
                (updatedModule, newSignals))
        // Update module inventory
        let newModuleMap = 
            updateList 
            |> List.map fst 
            |> List.fold (fun (stateMap: Map<string,Module>) (m: Module) ->
                let sid = moduleId m
                stateMap.Remove(sid).Add(sid,m)
            ) modules
        // update signal count
        let newSignals = 
            updateList 
            |> List.map snd
            |> List.concat
            |> List.filter (fun (_,_, st) -> st <> SignalType.None)
        let (lows, highs) = 
            newSignals
            |> List.fold (fun (lows: int64, highs: int64) (source,destination,signalType) ->
                match signalType with 
                | None -> (lows, highs)
                | High -> (lows, highs + 1L)
                | Low -> (lows + 1L, highs)
                ) (lowCount,highCount)
        HandlePulses newModuleMap newSignals (lows, highs)

let pushButton (modules: Map<string,Module>) lowPulses highPulses =
    let bc: Module = modules.["broadcaster"]
    let firstSignal = [("", (moduleId bc), SignalType.Low)]
    HandlePulses modules firstSignal (lowPulses + 1L,highPulses)

let buttonRampage (modules: Map<string,Module>) =
    let (finalModules, finalLows, finalHighs) = 
        [1..1000]
        |> List.fold (fun ((modules: Map<string,Module>),lowPulses, highPulses) i -> 
            let ((newModules: Map<string,Module>), (newLows: int64), (newHighs: int64)) = pushButton modules lowPulses highPulses
            (newModules, newLows, newHighs)
        ) (modules, 0L, 0L) 
    printfn "Lows: %A Highs: %A" finalLows finalHighs
    finalLows * finalHighs

let exampleModules1 = parseInput "./input/day20_example1.txt"
let exampleModules2 = parseInput "./input/day20_example2.txt"
let modules = parseInput "./input/day20.txt"

// These do work
buttonRampage exampleModules1 |> printfn "Example answer 1: %A"
buttonRampage exampleModules2 |> printfn "Example answer 2: %A"
// This does not
buttonRampage modules |> printfn "Answer 1: %A"