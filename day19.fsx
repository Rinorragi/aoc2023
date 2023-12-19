// Macro to get running time
#time 

let nl = "\n"

type Xmas = {
    x: int64
    m: int64
    a: int64 
    s: int64
}

type Rule = 
    | GreaterThan of comp: char * compareTo: int64 * result: string
    | LessThan of comp: char * compareTo: int64 * result: string
    | JumpToAnother of workflow: string
    | WorkFlowFinished of result: char

type Workflow = {
    id: string
    rules: Rule list
}

type WorkflowResult = {
    workflowId: string
    result: bool
    minXmas: Xmas
    maxXmas: Xmas
}

let parseInput (filePath) =
    let sInput = 
        System.IO.File.ReadAllText filePath
        // Replace CRLF to only LF (copy+paste and input in different format)
        |> fun s -> s.Replace("\r\n", nl)
        |> fun s -> 
            s.Split(nl+nl, System.StringSplitOptions.RemoveEmptyEntries)
    
    let workflows = 
        sInput.[0].Split(nl,System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map(fun s -> 
            let sWf = s.Split('{',System.StringSplitOptions.RemoveEmptyEntries)
            let ruleList = 
                sWf.[1]
                    .Substring(0, sWf.[1].Length - 1)
                    .Split(',',System.StringSplitOptions.RemoveEmptyEntries) 
                |> List.ofArray
                |> List.map (fun sRule -> 
                    match sRule.Length, sRule.Contains('<') || sRule.Contains('>') with 
                    | 1,_ -> if sRule.[0] = 'A' || sRule.[0] = 'R' then WorkFlowFinished (sRule.[0]) else failwith (sprintf "Unknown string %s:" sRule)
                    | _,true -> 
                        let sRuleSplitted = sRule.Split(':',System.StringSplitOptions.RemoveEmptyEntries)
                        let comparisonSplitted = sRuleSplitted.[0].Split([|'<';'>'|],System.StringSplitOptions.RemoveEmptyEntries)
                        let isGreaterThan = sRule.Contains('>')
                        if isGreaterThan 
                        then GreaterThan (comp = comparisonSplitted.[0].[0], compareTo = (comparisonSplitted.[1] |> int64), result = sRuleSplitted.[1])
                        else LessThan (comp = comparisonSplitted.[0].[0], compareTo = (comparisonSplitted.[1] |> int64), result = sRuleSplitted.[1])
                    | _ -> JumpToAnother (sRule)
                )
            {
                id = sWf.[0]
                rules = ruleList
            })
        |> List.map (fun wf -> 
            (wf.id, wf))
        |> Map.ofList
    let parts = 
        sInput.[1].Split(nl,System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map (fun s -> 
            let sXmas = s.Substring(1, s.Length - 2).Split(',')
            {
                x = sXmas.[0].Substring(2) |> int64
                m = sXmas.[1].Substring(2) |> int64
                a = sXmas.[2].Substring(2) |> int64
                s = sXmas.[3].Substring(2) |> int64
            })
    (workflows,parts)

let compareComponents (compChar: char) (compXmas: Xmas) (compInt: int64) compareFunc = 
    match compChar with 
    | 'x'-> compareFunc compXmas.x compInt
    | 'm'-> compareFunc compXmas.m compInt
    | 'a'-> compareFunc compXmas.a compInt
    | 's'-> compareFunc compXmas.s compInt
    | _ -> failwith (sprintf "Wrong char %c" compChar)

let rec solveSorting (workflows: Map<string,Workflow>) (currentWF: Workflow) (part: Xmas)=
    let result = 
        currentWF.rules 
        |> List.fold (fun (ruleState, skipToEnd) currentRule -> 
            if skipToEnd then (ruleState, skipToEnd)
            else 
                match currentRule with 
                | WorkFlowFinished(result = c) -> if c = 'A' then (true,true) else (false,true)
                | JumpToAnother(workflow = wf) -> (solveSorting workflows workflows.[wf] part, true)
                | GreaterThan(comp = c; compareTo = cint; result = r;) ->
                    if compareComponents c part cint (fun x y -> x > y)
                    then
                        match r with 
                        | "A" -> (true,true)
                        | "R" -> (false, true)
                        | _ -> (solveSorting workflows workflows.[r] part, true)
                    else 
                        (false,false)
                | LessThan (comp = c; compareTo = cint; result = r;) ->
                    if compareComponents c part cint (fun x y -> x < y)
                    then
                        match r with 
                        | "A" -> (true,true)
                        | "R" -> (false, true)
                        | _ -> (solveSorting workflows workflows.[r] part, true)
                    else 
                        (false,false)
                    
        ) (false,false)
    fst result 
let elfSortingMadness (workflows: Map<string,Workflow>) (parts: Xmas list) =
    parts
    |> List.map (fun x -> 
        let firstWorkflow = workflows.["in"]
        let result = solveSorting workflows firstWorkflow x
        (result, x))

let tuneMinMax (compChar: char) (value: int64) (minXmas: Xmas) (maxXmas: Xmas) (greaterThan: bool)= 
    match compChar, greaterThan with 
    | 'x', true -> ({ x = value + 1L; m = minXmas.m; a = minXmas.a; s = minXmas.s}, maxXmas) 
    | 'm', true -> ({ x = minXmas.x; m = value + 1L; a = minXmas.a; s = minXmas.s}, maxXmas) 
    | 'a', true -> ({ x = minXmas.x; m = minXmas.m; a = value + 1L; s = minXmas.s}, maxXmas) 
    | 's', true -> ({ x = minXmas.x; m = minXmas.m; a = minXmas.a; s = value + 1L}, maxXmas) 
    | 'x', false -> (minXmas, { x = value - 1L; m = maxXmas.m; a = maxXmas.a; s = maxXmas.s}) 
    | 'm', false -> (minXmas, { x = maxXmas.x; m = value - 1L; a = maxXmas.a; s = maxXmas.s}) 
    | 'a', false -> (minXmas, { x = maxXmas.x; m = maxXmas.m; a = value - 1L; s = maxXmas.s}) 
    | 's', false -> (minXmas, { x = maxXmas.x; m = maxXmas.m; a = maxXmas.a; s = value - 1L}) 
    | _ -> failwith (sprintf "Wrong char %c" compChar)

let calculateCombinations (minXmas: Xmas) (maxXmas: Xmas) = (maxXmas.x - minXmas.x + 1L) * (maxXmas.m - minXmas.m + 1L) * (maxXmas.a - minXmas.a + 1L) * (maxXmas.s - minXmas.s + 1L)
let rec solveDistinctCombinations (workflows: Map<string,Workflow>) (currentWF: Workflow) (minXmas: Xmas) (maxXmas: Xmas) =
    currentWF.rules 
    |> List.fold (fun (wfResults: WorkflowResult list, minState: Xmas, maxState: Xmas) currentRule -> 
        match currentRule with 
        | WorkFlowFinished(result = c) -> 
            let newResults = wfResults @ [{ workflowId = currentWF.id; result = (c = 'A') ; minXmas = minState; maxXmas = maxState }]
            (newResults , minState, maxState)
        | JumpToAnother(workflow = wf) -> 
            let (subResults, _, _) = solveDistinctCombinations workflows workflows.[wf] minState maxState
            (wfResults @ subResults, minState, maxState)
        | GreaterThan(comp = c; compareTo = cint; result = r;) ->
            let (calcMinState, calcMaxState) = tuneMinMax c cint minState maxState true
            let (newMinState, newMaxState) = tuneMinMax c (cint + 1L) minState maxState false
            match r with 
            | "A"
            | "R" -> 
                let newResults = wfResults @ [{ workflowId = currentWF.id; result = (r = "A"); minXmas = calcMinState; maxXmas = calcMaxState }]
                (newResults , newMinState, newMaxState)
            | _ -> 
                let (subResults, _, _) = (solveDistinctCombinations workflows workflows.[r] calcMinState calcMaxState)
                (wfResults @ subResults, newMinState, newMaxState)
        | LessThan (comp = c; compareTo = cint; result = r;) ->
            let (calcMinState, calcMaxState) = tuneMinMax c cint minState maxState false
            let (newMinState, newMaxState) = tuneMinMax c (cint - 1L) minState maxState true
            match r with 
            | "A"
            | "R" -> 
                let newResults = wfResults @ [{ workflowId = currentWF.id; result = (r = "A"); minXmas = calcMinState; maxXmas = calcMaxState }]
                (newResults , newMinState, newMaxState)
            | _ -> 
                let (subResults, _, _) = (solveDistinctCombinations workflows workflows.[r] calcMinState calcMaxState)
                (wfResults @ subResults, newMinState, newMaxState)
        ) ([], minXmas, maxXmas)

let solvePart1 (workflows: Map<string,Workflow>) (parts: Xmas list) =
    elfSortingMadness workflows parts
    |> List.filter fst
    |> List.map (fun (_,x) -> 
        x.x + x.m + x.a + x.s)
    |> List.sum

let solvePart2 (workflows: Map<string,Workflow>) =
    let (res,_,_) = solveDistinctCombinations workflows workflows.["in"] { x=1; m=1; a=1; s=1} {x=4000;m=4000;a=4000;s=4000}
    res |> List.map (fun s -> 
        let combinations = calculateCombinations s.minXmas s.maxXmas
        s.result, combinations)
    |> List.filter fst
    |> List.map snd
    |> List.sum

let (exampleWorkflows, exampleParts) = parseInput "./input/day19_example.txt"
let (workflows, parts) = parseInput "./input/day19.txt"
solvePart1 exampleWorkflows exampleParts |> printfn "Example answer 1: %d"
solvePart1 workflows parts |> printfn "Answer 1: %d"

solvePart2 exampleWorkflows |> printfn "Example answer 2: %A"
solvePart2 workflows |> printfn "Example answer 2: %A"