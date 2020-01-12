open System
open System.IO
open Checked
open System.Diagnostics
open System.Collections
open MyQueue
//https://adventofcode.com/2019/day/18

let parse (path:string) =
    [|  
        use sr = new StreamReader(path)
        while (not sr.EndOfStream) do
            yield sr.ReadLine().ToCharArray()
    |]

type Tree = {area:char; distance: int; branches: Tree[]; back: Tree option}

let findStart (map: char [] []) =
    let i2 =
        map
        |> Array.findIndex(Array.contains '@')
    let i1 = 
        map.[i2] 
        |> Array.findIndex ((=) '@')
    (i1,i2)

let getRect<'a> (map: 'a [][]) =
    (map.[0].Length - 1, map.Length - 1)

type Branch = {area: char; distance: int; position: (int * int)}
type Explore = {branches: Branch[]; visited: (int * int) list}

let getMoves (x:int,y:int) (xmax, ymax) (visited: (int * int) list) (map: char [][]) =
    let mutable additional_visited = visited
    let mutable N,S,E,W = false, false, false, false
    let mutable yN, yS, xE, xW = y + 1, y - 1, x + 1, x - 1
    let branches = 
        [|  
            //east
            while (not E && xE < xmax ) do
                if additional_visited |> List.contains (xE,y)
                then E <- true else 
                additional_visited <- (xE,y) :: additional_visited
                match map.[xE].[y] with
                | '.' ->
                    xE <- xE + 1
                | '#' -> 
                    E <- true
                | area ->
                    E <- true
                    yield {area = area; distance = xE - x; position = (xE, y)}
            //to do west, north, sud
        |]
    {branches = branches; visited = additional_visited}

let space = '.'

let findBranches (map: char [] []) (visited: (int * int) list) (position: int * int) 
    : Explore =
    // TO-DO
    {branches = [||]; visited = []}


(*

#########
#b.A.@.a#
#########
So, collecting every key took a total of 8 steps.



Here is a larger example:

########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################

Finally, collect key e to unlock door E, then collect key f, taking a grand total of 86 steps.

Here are a few more examples:

########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################
Shortest path is 132 steps: b, a, c, d, f, e, g

#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################
Shortest paths are 136 steps;
one is: a, f, b, j, g, n, h, d, l, o, e, p, c, i, k, m

########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################
Shortest paths are 81 steps; one is: a, c, f, i, d, g, b, e, h

*)


let rec buildTree (map: char [] []) (visited: (int * int) list) (branch: Branch) : Tree =
    let expl = findBranches map visited branch.position
    if  (expl.branches = [||]) 
    then {area = branch.area; distance = branch.distance; branches = [||]; back = None } else
    let visited = expl.visited
    {area = branch.area; distance = branch.distance; branches = 
        [|
            for branch in expl.branches do
                yield buildTree map visited branch
        |] ; back = None
        }

type Solution = {keys: char list; tree: Tree }

let otherBranches (i:int) (solution: Solution) : Tree[] =
    let branches = solution.tree.branches
    let distance = solution.tree.distance + branches.[i].distance
    [|0..branches.Length-1|]
    |> Array.except [|i|]
    |> Array.map(fun other ->
        {branches.[other] with distance = distance +  branches.[i].distance}
    )

type Step = | KeyStep | SpaceStep

let next (step:Step) (i:int) (solution: Solution) : Solution =
    let branches = solution.tree.branches
    let distance = solution.tree.distance + branches.[i].distance
    let area = branches.[i].area    
    let keys = area :: solution.keys
    let branches, back =
        match step with
        | SpaceStep ->
            otherBranches i solution, Some  branches.[i]
        | KeyStep ->
            otherBranches i solution 
            |> Array.append branches.[i].branches, None
            
    {keys=keys; tree = {area = space; distance = distance; branches = branches; back = back} }

let rec findSolution (solution: Solution) : Solution option =
    let branches = solution.tree.branches
    if  (branches = [||] ) then Some solution else
    let alternatives : Solution [] =
        [|
            for i in 0..branches.Length-1 do
                if branches.[i].area = '#' then () else
                if (Char.IsLower branches.[i].area) then
                    let solution = next KeyStep i solution
                    match findSolution solution with
                    | Some solution -> yield solution
                    | None -> ()
                if (Char.IsUpper branches.[i].area) then
                    let needed = Char.ToLower branches.[i].area
                    if solution.keys |> List.contains needed |> not then () else
                    let solution = next SpaceStep i solution
                    match findSolution solution with
                    | Some solution -> yield solution
                    | None -> ()
                if branches.[i].area = space then
                    let solution = next SpaceStep i solution
                    match findSolution solution with
                    | Some solution -> yield solution
                    | None -> ()
        |]
    match alternatives with
    | [||] -> None
    | alternatives ->
        alternatives |> Array.minBy(fun a -> a.tree.distance) |> Some

[<EntryPoint>]
let main _ =

    let sw = Stopwatch()
    sw.Start()

    let map = parse @"C:\dev\FSharp\AoC2019\Day18\input_demo.txt"

    printfn "executed in %d ms"  sw.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    