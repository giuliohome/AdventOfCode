module Day

open Common

let lines = getLines "input.txt"
     
let liner (line:string)  = line.ToCharArray()

let move1 = [|[|0..127|]; [|0..8|]|]

let read1 f take _skip array nums = 
        nums
        |> Array.fold
            (fun s t -> 
                s
                )
            array
        |> Array.head

let step1 read1 cols rows move (state:State) nums =
    
    let result1 = 
        read1 Array.take 'F' 'B'
            (move:int[][]).[0] nums
    let result2 = 
        read1 Array.skip 'L' 'R'
            (move:int[][]).[1] nums

    let result = result1 + result2

    let curr_list = result :: state.curr_list
    
    {state with
        curr_list = curr_list; 
        curr_row=state.curr_row + 1}   


let phase1 (lines: string[]) =
    let curr_list = phase (step1 read1) move1 liner lines
    curr_list |> List.head

let move2 = move1
let step2 = step1
let read2 = read1

let phase2  (lines: string[]) =
    let curr_list = phase (step2 read2) move2 liner lines 
    curr_list |> List.head
    
