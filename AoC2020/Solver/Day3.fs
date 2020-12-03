module Day3

open Common

let lines = getLines "input_day03.txt"
     
let liner (line:string) : bool[] = line.ToCharArray() |> Array.map(fun x -> x = '#') 

let firstLineTest = liner lines.[0]




type Move = { right:int; down:int}
let move1 = {right=3; down=1}
type State = {result:int; xpos:int; ypos:int; curr_row:int}
let start ={result=0; xpos=0; ypos=0; curr_row=0}

let step cols rows (move:Move) (debug:bool) (state:State) (nums:'a) =
    if debug 
    then
        Array.iteri (fun i x -> 
            match i, x with
            | i, true when i = state.xpos -> printf "X"
            | i, false when i = state.xpos -> printf "0"
            | _, true -> printf "#"
            |_ -> printf ".")
            nums
        printfn ""
    if state.curr_row < state.ypos
    then {state with curr_row = state.curr_row + 1}
    else 
    let result =
        if (nums: bool[]).[state.xpos]
        then state.result + 1
        else state.result
    let xpos = (state.xpos + move.right) % cols
    let ypos = (state.ypos + move.down) % rows
    {result=result; xpos=xpos; ypos=ypos; curr_row=state.curr_row + 1}


let step1 cols rows (debug:bool) (state:State) (nums:'a) =
    step cols rows move1 debug state nums

let phase1 (debug:bool) (lines: string[]) =
    let cols = lines.[0].Length
    let rows = lines.Length
    let s = 
       (if debug then debugger else solver) 
           liner (step1 cols rows) start lines 
    s.result

let phase2 (debug:bool) (lines: string[]) =
    let cols = lines.[0].Length
    let rows = lines.Length
    [|
        {right=1; down=1}
        {right=3; down=1}
        {right=5; down=1}
        {right=7; down=1}
        {right=1; down=2}
    |]
    |> Array.map (fun move -> 
        let s = 
            (if debug then debugger else solver) 
                liner (step cols rows move) start  lines
        if debug
        then printfn "result %d" s.result
        s.result
       ) 
    |> Array.fold (*) 1 

    


let test = @"..##.........##.........##.........##.........##.........##.......
#..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....
.#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........X.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...#X....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#"
            .Replace("O",".").Replace("X","#").Split('\n') 
            |> Array.map(cleanLine)