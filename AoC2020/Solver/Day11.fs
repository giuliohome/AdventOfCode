module Day11

open Common

let lines = getLines "input_day11.txt" 
     
let liner (line:string)  = 
    line.ToCharArray()


let countOccupied (nums: char[][]) : int =
    nums
    |> Array.fold
        (fun s t -> 
            t 
            |> Array.filter ((=) '#')
            |> Array.length
            |> (+) s
        )
        0

let see1coord (nums: char[][]) (from: int*int) (i:int) (j_arr: int[]) : (int*int)[] =
        [|
        for j in j_arr do
         yield i,j
        |]

let see1 (from: int*int) (nums: char[][]) (i:int) (j_arr: int[]) =
        [|
        for j in j_arr do
         yield nums.[i].[j]
        |]

let rec see2coord (nums: char[][]) (from: int*int) (i:int) (j_arr: int[]) : (int*int) [] =
        [|
        for j in j_arr do
        yield match nums.[i].[j] with
         | 'L' -> (i,j)
         | '#' -> (i,j)
         | '.' -> 
            let i0, j0 = from
            let idelta, jdelta = i - i0, j - j0
            let inext, jnext = i + idelta, j + jdelta
            if inext >= 0 && inext <= nums.Length - 1 && jnext >= 0 && jnext <= nums.[0].Length - 1
            then (see2coord (nums: char[][]) (i,j) inext [|jnext|]).[0]
            else  (i,j)
         | other -> failwith <| sprintf "unexpected %c" other
         |]

let rec see2 (from: int*int) (nums: char[][]) (i:int) (j_arr: int[]) =
        [|
        for j in j_arr do
        yield match nums.[i].[j] with
         | 'L' -> 'L'
         | '#' -> '#'
         | '.' -> 
            let i0, j0 = from
            let idelta, jdelta = i - i0, j - j0
            let inext, jnext = i + idelta, j + jdelta
            if inext >= 0 && inext <= nums.Length - 1 && jnext >= 0 && jnext <= nums.[0].Length - 1
            then (see2 (i,j) nums inext [|jnext|]).[0]
            else '.'
         | other -> failwith <| sprintf "unexpected %c" other
         |]

         
let findAdjacentCoord (see: int*int -> int -> int[] -> (int*int)[]) rows cols (i:int) (j:int) : (int*int)[][] =
    [|
    match (i>0), (i<rows - 1), (j>0),(j < cols - 1) with
    | true, true, true, true -> 
        yield see (i,j) (i-1) [|j-1 .. j+1|]
        yield see (i,j) i  [|j-1; j+1|]
        yield see (i,j) (i+1) [|j-1 .. j+1|]
    | true, true, false, _ -> 
        yield see (i,j) (i-1) [|j .. j+1|]
        yield see (i,j) i  [|j+1|]
        yield see (i,j) (i+1) [|j .. j+1|]
    | true, true, _, false -> 
        yield see (i,j) (i-1) [|j-1 .. j|]
        yield see (i,j) i  [|j-1|]
        yield see (i,j) (i+1) [|j-1 .. j|]
    | false, _, true, true -> 
        yield see (i,j) i  [|j-1; j+1|]
        yield see (i,j) (i+1) [|j-1 .. j+1|]
    | false, _, false, _ -> 
        yield see (i,j) i  [|j+1|]
        yield see (i,j) (i+1) [|j .. j+1|]
    | false, _, _, false -> 
        yield see (i,j) i  [|j-1|]
        yield see (i,j) (i+1) [|j-1 .. j|]
    | _, false, true, true -> 
        yield see (i,j) (i-1) [|j-1 .. j+1|]
        yield see (i,j) i  [|j-1; j+1|]
    | _, false, false, _ -> 
        yield see (i,j) (i-1) [|j .. j+1|]
        yield see (i,j) i  [|j+1|]
    | _, false, _, false -> 
        yield see (i,j) (i-1) [|j-1|]
        yield see (i,j) i  [|j-1; j|]
    |]

let findAdjacent (see: int*int -> char[][] -> int -> int[] -> char[]) (nums: char[][]) (i:int) (j:int) : char[][] =
    [|
    match (i>0), (i<nums.Length - 1), (j>0),(j < nums.[0].Length - 1) with
    | true, true, true, true -> 
        yield see (i,j) nums (i-1) [|j-1 .. j+1|]
        yield see (i,j) nums i  [|j-1; j+1|]
        yield see (i,j) nums (i+1) [|j-1 .. j+1|]
    | true, true, false, _ -> 
        yield see (i,j) nums (i-1) [|j .. j+1|]
        yield see (i,j) nums i  [|j+1|]
        yield see (i,j) nums (i+1) [|j .. j+1|]
    | true, true, _, false -> 
        yield see (i,j) nums (i-1) [|j-1 .. j|]
        yield see (i,j) nums i  [|j-1|]
        yield see (i,j) nums (i+1) [|j-1 .. j|]
    | false, _, true, true -> 
        yield see (i,j) nums i  [|j-1; j+1|]
        yield see (i,j) nums (i+1) [|j-1 .. j+1|]
    | false, _, false, _ -> 
        yield see (i,j) nums i  [|j+1|]
        yield see (i,j) nums (i+1) [|j .. j+1|]
    | false, _, _, false -> 
        yield see (i,j) nums i  [|j-1|]
        yield see (i,j) nums (i+1) [|j-1 .. j|]
    | _, false, true, true -> 
        yield see (i,j) nums (i-1) [|j-1 .. j+1|]
        yield see (i,j) nums i  [|j-1; j+1|]
    | _, false, false, _ -> 
        yield see (i,j) nums (i-1) [|j .. j+1|]
        yield see (i,j) nums i  [|j+1|]
    | _, false, _, false -> 
        yield see (i,j) nums (i-1) [|j-1|]
        yield see (i,j) nums i  [|j-1; j|]
    |]
    

let read occupiedSeats (findAdjacentCoordN: int -> int -> (int*int)[][]) (nums: char[][])  =
    let cols = nums.[0].Length
    let rows = nums.Length

     
    [|
    for i in [0..rows-1] do
        yield 
            [|
            for j in [0..cols-1] do
                let adj = 
                    findAdjacentCoordN i j
                    |> Array.map(fun x -> x |> Array.map (fun (ii,jj) -> nums.[ii].[jj]) )
                    |> countOccupied
                yield match nums.[i].[j] with
                | 'L' ->  if adj = 0 then '#' else 'L'
                | '#' ->  if adj >=occupiedSeats then 'L' else '#'
                | '.' -> '.'
                | other -> failwith <| sprintf "unexpected %c" other
            |]
    |]


    
        
        
let stepN (readN: char[][] -> char[][]) (state:State) nums =


    let mutable next  = readN nums
    let mutable current = nums

    while next |> Array.forall2 (=) current |> not do
        current <- next
        next <- readN current
    
    {state with
        result = countOccupied current
        curr_row = state.curr_row + 1;}   

let nums0 = 
    lines
    |> Array.map liner        

let mapCreate seecoord: Map<int*int, (int*int)[][]> =
    let test =
        [|0..nums0.Length-1|]
        |> Array.map(fun x -> 
            [|0.. nums0.[0].Length-1|]
            |> Array.map(fun y ->
                (x,y)
            )
        )
        |> Array.concat
    test
    |> Array.map (fun (x,y) -> (x,y), findAdjacentCoord (seecoord nums0) nums0.Length nums0.[0].Length x y)
    |> Map.ofArray

let mapper1 = mapCreate see1coord
let mapper2 = mapCreate see2coord

let read1 = read 4 (fun x y -> mapper1.[(x,y)])
let read2  = read 5 (fun x y -> mapper2.[(x,y)])
let phaseN readN (lines: string[]) =
    lines
    |> Array.map liner
    |> stepN readN start 
    |> fun s -> s.result

let phase1 = phaseN read1 
let phase2  = phaseN read2
    
