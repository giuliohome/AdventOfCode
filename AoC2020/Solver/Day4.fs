module Day4

open Common

let lines0 = getLines "input_day04.txt"

let txtFields = @"byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)"
// cid (Country ID)" 

let fields = 
    txtFields.Split('\n')
    |> Array.map cleanLine
    |> Array.map (fun l -> l.Split(' ').[0])

// fields |> Array.iter (printfn "%A") 
     
let liner (line:string)  = 
    line.Split(' ')
    |> Array.map(fun f -> f.Split(':').[0])
    |> Array.toList


let lines =
    lines0
    |> Array.fold (fun s t -> 
        if System.String.IsNullOrWhiteSpace(t)
        then t :: s
        else 
        match s with
        | (f::r) -> System.String.Join(' ', [f.Trim() ; t.Trim()]) :: r
        )
        [""]
    |> List.toArray

// liner lines.[0] |> Array.iter (printfn "%A") 

let move1 = [|[|0..127|]; [|0..8|]|]

let read1 nums = 
        fields
        |> Array.forall (fun f -> nums |> List.contains f)
        |> function | true -> 1 | false -> 0

let step1 read1 cols rows move (state:State) nums =
    
    let result1 = 
        read1 nums

    let result = result1 

    let curr_list = result :: state.curr_list
    
    {state with
        curr_list = curr_list; 
        curr_row=state.curr_row + 1}   


let phase1 (lines: string[]) =
    let curr_list = phase (step1 read1) move1 liner lines
    curr_list |> List.sum

let move2 = move1
let step2 = step1
let read2 = read1

let phase2  (lines: string[]) =
    let curr_list = phase (step2 read2) move2 liner lines 
    curr_list |> List.head
    
