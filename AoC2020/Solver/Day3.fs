module Day3

open Common

let lines = getLines "input_day03.txt"
     
let liner (line:string) : bool[] = line.ToCharArray() |> Array.map(fun x -> x = '#') 

let firstLineTest = liner lines.[0]



let cols = lines.[0].Length
let rows = lines.Length

type Move = { right:int; down:int}
let move1 = {right=3; down=1}
type State = {result:int; xpos:int}
let start ={result=0; xpos=0}

let step1 (debug:bool) (state:State) (nums:'a) =
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
    let result =
        if (nums: bool[]).[state.xpos]
        then state.result + 1
        else state.result
    let xpos = (state.xpos + move1.right) % cols
    {result=result; xpos=xpos}

let step2 (counter:'s) (nums:'a) =

    counter

    


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