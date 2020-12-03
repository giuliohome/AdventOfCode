namespace FSharpSPA
open System.IO
open Year2020Common
open WebSharper
[<JavaScript>]
module Year2020Day3 = 

    
    
     
    let liner (line:string) : bool[] = line.ToCharArray() |> Array.map(fun x -> x = '#') 






    type Move = { right:int; down:int}
    let move1 = {right=3; down=1}
    type State = {result:int; xpos:int; ypos:int; curr_row:int}
    let start ={result=0; xpos=0; ypos=0; curr_row=0}

    let step cols rows (move:Move) (state:State) (nums:bool[]) =
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


    let step1 cols rows (state:State) (nums:bool[]) =
        step cols rows move1 state nums

    let phase1 (lines: string[]) =
        let cols = lines.[0].Length
        let rows = lines.Length
        JavaScript.Console.Log("cols",cols, "rows",rows)
        let s = 
           solver
               liner (step1 cols rows) start lines 
        s.result

    let phase2 (lines: string[]) =
        let cols = lines.[0].Length
        let rows = lines.Length
        JavaScript.Console.Log("cols",cols, "rows",rows)
        [|
            {right=1; down=1}
            {right=3; down=1}
            {right=5; down=1}
            {right=7; down=1}
            {right=1; down=2}
        |]
        |> Array.map (fun move -> 
            let s = 
                solver 
                    liner (step cols rows move) start  lines
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