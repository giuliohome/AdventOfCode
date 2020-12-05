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
    |> Array.map(fun f -> f.Split(':'))
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
        |> Array.forall (fun f -> nums |> List.map Array.head |> List.contains f)
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
let read2 nums = 
        fields
        |> Array.forall (fun f -> 
            
            nums 
            |> List.exists(fun (num: string[]) -> 
                num.[0] =  f &&
                match num.[0] with
                | "byr" -> 
                    match System.Int32.TryParse num.[1] with
                    | false, _ -> false
                    | true, i -> i >= 1920 && i <= 2002
                | "iyr" -> 
                    match System.Int32.TryParse num.[1] with
                    | false, _ -> false
                    | true, i -> i >= 2010 && i <= 2020
                | "eyr" -> 
                    match System.Int32.TryParse num.[1] with
                    | false, _ -> false
                    | true, i -> i >= 2020 && i <= 2030
                | "hgt" -> 
                    if num.[1].EndsWith("cm") then
                        match System.Int32.TryParse (num.[1].Substring(0,num.[1].Length-2)) with
                        | false, _ -> false
                        | true, i -> i >= 150 && i <= 193
                    elif num.[1].EndsWith("in") then
                        match System.Int32.TryParse (num.[1].Substring(0,num.[1].Length-2)) with
                        | false, _ -> false
                        | true, i -> i >= 59 && i <= 76
                    else false
                | "hcl" -> 
                    num.[1].Length = 7 &&
                    match num.[1].ToCharArray() |> Array.toList with
                        | ('#'::r) -> 
                            match r with
                            | r when 
                                r 
                                |> List.forall(fun r -> 
                                    ((int r) >= (int 'a') && (int r) <= (int 'f')) 
                                    ||
                                    ((int r) >= (int '0') && (int r) <= (int '9')) ) -> true
                            | _ -> 
                                System.Diagnostics.Debug.WriteLine("hcl: " + num.[1])
                                false
                        | _ -> false
                | "ecl" -> 
                    
                    match num.[1] with
                    | "amb" 
                    | "blu" 
                    | "brn" 
                    | "gry" 
                    | "grn" 
                    | "hzl" 
                    | "oth" -> true
                    | _ -> 
                        System.Diagnostics.Debug.WriteLine("ecl: " + num.[1])
                        false
                | "pid" -> 
                    //System.Diagnostics.Debug.Write(num.[1])
                    let res =
                        (num.[1].Length) = 9 && 
                            num.[1].ToCharArray()
                            |> Array.forall(fun r -> 
                                            (int r) >= (int '0') 
                                            && (int r) <=  (int '9'))
                    //System.Diagnostics.Debug.WriteLine(": " + res.ToString())
                    res
                | "cid" -> 
                    true
                )
            )
        |> function | true -> 1 | false -> 0

let phase2  (lines: string[]) =
    let curr_list = phase (step2 read2) move2 liner lines 
    curr_list |> List.sum
    
