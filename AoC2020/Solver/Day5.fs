module Day5

open Common

let lines = getLines "input_day05.txt"
     
let liner (line:string)  = line.ToCharArray()

let firstLineTest = liner lines.[0]

let move1 = [|[|0..127|]; [|0..8|]|]
type State = {result:int; xpos:int; ypos:int; curr_row:int; curr_list: int list}
let start ={result=0; xpos=0; ypos=0; curr_row=0; curr_list = []}

let step1 cols rows move (state:State) nums =
    if state.curr_row < state.ypos
    then {state with curr_row = state.curr_row + 1}
    else 
    let result1 = 
        (nums: char[])
        |> Array.take 7
        |> Array.fold
            (fun s t -> 
                let l0 :int = (s:int[]).Length / 2
                match t with
                | 'F' -> s |> Array.take l0
                | 'B' -> s |> Array.skip l0
                )
            (move:int[][]).[0]
        |> Array.head
    let result2 = 
        (nums: char[])
        |> Array.skip 7
        |> Array.fold
            (fun s t -> 
                let l0 :int = (s:int[]).Length / 2
                match t with
                | 'L' -> s |> Array.take l0
                | 'R' -> s |> Array.skip l0
                )
            (move:int[][]).[1]
        |> Array.head
    let result = 8*result1 + result2

    {state with
        result= max state.result  result; curr_row=state.curr_row + 1}

let move2 = move1
let step2 cols rows move (state:State) nums =
    if state.curr_row < state.ypos
    then {state with curr_row = state.curr_row + 1}
    else 
    let result1 = 
        (nums: char[])
        |> Array.take 7
        |> Array.fold
            (fun s t -> 
                let l0 :int = (s:int[]).Length / 2
                match t with
                | 'F' -> s |> Array.take l0
                | 'B' -> s |> Array.skip l0
                )
            (move:int[][]).[0]
        |> Array.head
    let result2 = 
        (nums: char[])
        |> Array.skip 7
        |> Array.fold
            (fun s t -> 
                let l0 :int = (s:int[]).Length / 2
                match t with
                | 'L' -> s |> Array.take l0
                | 'R' -> s |> Array.skip l0
                )
            (move:int[][]).[1]
        |> Array.head
    let result = 8*result1 + result2

    let curr_list = result :: state.curr_list
    
    {state with
        curr_list = curr_list; 
        curr_row=state.curr_row + 1}

let phase1 (lines: string[]) =
    let cols = lines.[0].Length
    let rows = lines.Length
    let s = 
       solver
           liner (step1 cols rows move1) start lines 
    s.result

let phase2  (lines: string[]) =
    let cols = lines.[0].Length
    let rows = lines.Length
    let s = 
       solver
           liner (step2 cols rows move2) start lines 
    //s.curr_list
    //|> List.max
    let my =
        [s.curr_list |> List.min .. s.curr_list |> List.max]
        |> List.filter (fun x -> 
            (s.curr_list |> List.contains (x - 1) )
            && (s.curr_list |> List.contains (x + 1))
            && (s.curr_list |> List.contains x |> not)
        )
    my |> List.iter(printfn "%d")
    my |> List.head
    
