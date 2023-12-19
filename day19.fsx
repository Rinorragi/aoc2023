// Macro to get running time
#time 

let nl = "\n"

type Xmas = {
    x: int
    m: int
    a: int 
    s: int
}

type Rule = 
    | GreaterThan of comp: char * compareTo: int * result: string
    | LessThan of comp: char * compareTo: int * result: string
    | JumpToAnother of workflow: string
    | WorkFlowFinished of result: char

type Workflow = {
    id: string
    rules: Rule list
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
                        then GreaterThan (comp = comparisonSplitted.[0].[0], compareTo = (comparisonSplitted.[1] |> int), result = sRuleSplitted.[1])
                        else LessThan (comp = comparisonSplitted.[0].[0], compareTo = (comparisonSplitted.[1] |> int), result = sRuleSplitted.[1])
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
                x = sXmas.[0].Substring(2) |> int
                m = sXmas.[1].Substring(2) |> int
                a = sXmas.[2].Substring(2) |> int
                s = sXmas.[3].Substring(2) |> int
            })
    (workflows,parts)

let compareComponents (compChar: char) (compXmas: Xmas) (compInt: int) compareFunc = 
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

let solvePart1 (workflows: Map<string,Workflow>) (parts: Xmas list) =
    elfSortingMadness workflows parts
    |> List.filter fst
    |> List.map (fun (_,x) -> 
        x.x + x.m + x.a + x.s)
    |> List.sum

let (exampleWorkflows, exampleParts) = parseInput "./input/day19_example.txt"
let (workflows, parts) = parseInput "./input/day19.txt"
solvePart1 exampleWorkflows exampleParts |> printfn "Example answer 1: %d"
solvePart1 workflows parts |> printfn "Answer 1: %d"
