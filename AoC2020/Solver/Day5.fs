module Day5

open Common

let lines = getLines "input_day05.txt"
     
let liner (line:string)  = line.ToCharArray()

let firstLineTest = liner lines.[0]

let move1 = [|[|0..127|]; [|0..8|]|]
type State = {result:int; xpos:int; ypos:int; curr_row:int; curr_list: int list}
let start ={result=0; xpos=0; ypos=0; curr_row=0; curr_list = []}

let step1 cols rows move (state:State) nums =
    let read f take _skip array  = 
        (nums: char[])
        |> f 7
        |> Array.fold
            (fun s t -> 
                let l0 :int = (s:int[]).Length / 2
                if t = take
                then s |> Array.take l0
                else s |> Array.skip l0
                )
            array
        |> Array.head

    let result1 = 
        read Array.take 'F' 'B'
            (move:int[][]).[0]
    let result2 = 
        read Array.skip 'L' 'R'
            (move:int[][]).[1]

    let result = 8*result1 + result2

    let curr_list = result :: state.curr_list
    
    {state with
        curr_list = curr_list; 
        curr_row=state.curr_row + 1}

let move2 = move1
let step2 = step1
    
let phase (lines: string[]) =
    let cols = lines.[0].Length
    let rows = lines.Length
    let s = 
       solver
           liner (step1 cols rows move1) start lines 
    s.curr_list

let phase1 (lines: string[]) =
    phase lines
    |> List.max

let phase2  (lines: string[]) =
    let curr_list = phase lines 
    let my =
        [curr_list |> List.min .. curr_list |> List.max]
        |> List.filter (fun x -> 
            (curr_list |> List.contains (x - 1) )
            && (curr_list |> List.contains (x + 1))
            && ( curr_list |> List.contains x |> not)
        )
    my |> List.iter(printfn "%d")
    my |> List.head
    
