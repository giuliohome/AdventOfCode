module Day6

open Common

let lines0 = getContent "input_day06.txt" |> cleanLastEmptyLine
     
let liner (line:string)  = 
    line.Split('\n')


let lines =
    lines0.Split([|"\n\n"|], System.StringSplitOptions.None)
    

let read (set_op: Set<char> -> Set<char> -> Set<char>) (nums: string[])  = 
        let set0 = Set(nums.[0].ToCharArray())
        nums
        |> Array.skip 1
        |> Array.fold
            (fun s t ->
                t.ToCharArray() 
                |> Set
                |> set_op s)
            set0
        |> Set.count

let read1  = read Set.union
let step1 read1 _cols _rows _move (state:State) nums =
    
    let result1 = 
        read1 nums
    
    {state with
        result = state.result + result1
        curr_row = state.curr_row + 1;}   

let move1 = None
let phase1 (lines: string[]) =
    phaseResult (step1 read1) move1 liner lines

let move2 = move1
let step2 = step1
let read2 = 
        read Set.intersect

let phase2  (lines: string[]) =
    phaseResult (step2 read2) move2 liner lines 
    
