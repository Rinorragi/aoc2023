// Macro to get running time
#time 

let nl = "\n"

type SpringRow = 
    struct 
        val original: string
        val config: int64 list
        val springs: char list
        new(orig, c, s) = {original = orig; config = c; springs = s }
    end

type PermutationStruct = 
    struct
        val springs: char list
        val configs: int64 list
        val groupStepper: int64
        val groupFinished: bool
        new (s, c, gs, gf) = {springs = s; configs = c; groupStepper = gs; groupFinished = gf }
    end 

let printSprintRow (sr: SpringRow) = printfn "Orig: %s, Conf: %A, Group: %A" sr.original sr.config sr.springs

let parseInput (filePath) =
    System.IO.File.ReadAllText filePath
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> List.map (fun s -> 
        let row = s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        new SpringRow(
            s,
            row.[1].Split(",",System.StringSplitOptions.RemoveEmptyEntries)
                |> List.ofArray
                |> List.map int64,
            row.[0].ToCharArray() |> List.ofArray))

// Caching
let memoize f =
    let dict = new System.Collections.Generic.Dictionary<_,_>()
    fun n ->
        match dict.TryGetValue(n) with
        | (true, v) -> v
        | _ ->
            let temp = f(n)
            dict.Add(n, temp)
            temp

#nowarn "40" // Ignore warning about recursive ojects instead of recrusive functions
// With help from reddit, we learned a ton about pattern matching today
let rec calculatePermutations =
    memoize(fun (ps: PermutationStruct) -> 
        match ps.springs, ps.configs, ps.groupStepper, ps.groupFinished with
        | [], [], 0L, _ -> 1L // Found valid
        // Dunno if damaged
        | '?' :: springTail, confHead :: confTail, 0L, false -> 
            // Damaged branch
            let damagedBranch = calculatePermutations (new PermutationStruct(springTail,confTail,(confHead - 1L),(confHead = 1L)))
            // Non-damaged branch
            let nonDamagedBranch = (calculatePermutations (new PermutationStruct(springTail,ps.configs,0L,false)))
            damagedBranch + nonDamagedBranch
        // Non-damaged
        | '?' :: springTail, [], 0L, false
        | '?' :: springTail, _, 0L, true
        | '.' :: springTail, _, 0L, _ -> calculatePermutations (new PermutationStruct(springTail,ps.configs,0L,false))
        // Damaged
        | '#' :: springTail, confHead :: confTail, 0L, false -> calculatePermutations (new PermutationStruct(springTail,confTail,(confHead - 1L),(confHead = 1L)))
        | '?' :: springTail, _, groupStep, false
        | '#' :: springTail, _, groupStep, false -> calculatePermutations (new PermutationStruct(springTail,ps.configs,(groupStep - 1L),(groupStep = 1L)))
        // Everything else is impossible
        | _ -> 0L)

let permutationSum (pretext: string ) (springs: SpringRow list) =
    springs
    |> List.mapi (fun i sr -> 
        let result =  calculatePermutations (new PermutationStruct(sr.springs,sr.config,0,false)) 
        result)
    |> List.sum
    |> printfn "%s %A" pretext 

let unfoldSprings (springs: SpringRow list) =
    springs
    |> List.map (fun sr -> 
        new SpringRow(
            sr.original,
            sr.config @ sr.config @ sr.config @ sr.config @ sr.config,
            sr.springs @ ['?'] @ sr.springs @ ['?'] @ sr.springs @ ['?'] @ sr.springs @ ['?'] @ sr.springs))

let examples = parseInput "./input/day12_example.txt" 
let springrows = parseInput "./input/day12.txt" 

permutationSum "Example answer 1: %A" examples
permutationSum "Answer 1: %A" springrows
//examples |> unfoldSprings |> permutationSum  "Example answer 2: %A"
springrows |> unfoldSprings |> permutationSum  "Answer 2: %A"