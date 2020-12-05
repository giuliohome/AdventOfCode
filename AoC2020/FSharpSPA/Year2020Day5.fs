namespace FSharpSPA
open System.IO
open Year2020Common
open WebSharper
[<JavaScript>]
module Year2020Day5 =
     
    let liner (line:string)  = line.ToCharArray()

    let move1 = [|[|0..127|]; [|0..8|]|]

    let read1 f take _skip array (nums: char[]) = 
            nums
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

    let step1 read1 cols rows move (state:State) nums =
    
        let result1 = 
            read1 Array.take 'F' 'B'
                (move:int[][]).[0] nums
        let result2 = 
            read1 Array.skip 'L' 'R'
                (move:int[][]).[1] nums

        let result = 8*result1 + result2

        let curr_list = result :: state.curr_list
    
        {state with
            curr_list = curr_list; 
            curr_row=state.curr_row + 1}   


    let phase1 (lines: string[]) =
        phase (step1 read1) move1 liner lines
        |> List.max

    let move2 = move1
    let step2 = step1
    let read2 = read1

    let phase2  (lines: string[]) =
        let curr_list = phase (step2 read2) move2 liner lines 
        let my =
            [curr_list |> List.min .. curr_list |> List.max]
            |> List.filter (fun x -> 
                (curr_list |> List.contains (x - 1) )
                && (curr_list |> List.contains (x + 1))
                && ( curr_list |> List.contains x |> not)
            )
        my |> List.iter(printfn "%d")
        my |> List.head
    
