open System
open System.IO
open Checked
open System.Diagnostics
//https://adventofcode.com/2019/day/18

let parse (path:string) =
    [|  
        use sr = new StreamReader(path)
        while (not sr.EndOfStream) do
            yield sr.ReadLine().ToCharArray()
    |]

type Tree = {area:char; distance: int; branches: Tree[]; back: Tree option}
[<Literal>]
let Space = '.'
[<Literal>]
let Wall = '#'
[<Literal>]
let Entrance = '@'

let findStart (map: char [] []) =
    let i2 =
        map
        |> Array.findIndex(Array.contains Entrance)
    let i1 = 
        map.[i2] 
        |> Array.findIndex ((=) Entrance)
    (i1,i2)

let getRect<'a> (map: 'a [][]) =
    (map.[0].Length - 1, map.Length - 1)

type Branch = {area: char; distance: int; position: (int * int)}
type Explore = {branches: Branch[]; visited: (int * int) list}

let getMoves (x:int,y:int) (xmax, ymax) (visited: (int * int) list) (map: char [][]) =
    [|
        if (x<xmax) && (visited |> List.contains (x+1,y) |> not) && (map.[y].[x+1] <> Wall)
        then yield (x+1,y)
        if (x>0) && (visited |> List.contains (x-1,y) |> not) && (map.[y].[x-1] <> Wall) 
        then yield (x-1,y)
        if (y<ymax) && (visited |> List.contains (x,y+1) |> not)  && (map.[y+1].[x] <> Wall)
        then yield (x,y+1)
        if (y>0) && (visited |> List.contains (x,y-1) |> not) && (map.[y-1].[x] <> Wall) 
        then yield (x,y-1)
    |]

let findBranchesGo (x:int,y:int) (xmax, ymax) (visited: (int * int) list) (map: char [][]) =
    let moves = getMoves (x,y) (xmax, ymax) visited map
    let mutable additional_visited = visited
    let branches = 
        [|
            for (xDir,yDir) in moves do
                //find the channel (= branch) up to next crossing (= tree node)
                //start with east and do the same with do west, north, sud 
                let mutable E, xE, yE, distanceE = false, xDir, yDir, 1
                while (not E) do
                    if  additional_visited |> List.contains (xE,yE)
                    then E <- true else 
                    additional_visited <- (xE,yE) :: additional_visited
                    match map.[yE].[xE] with
                    | Space ->
                        let goEast = getMoves (xE,yE) (xmax, ymax) additional_visited map
                        match goEast.Length with
                        | 0 ->
                            E <- true
                        | 1 ->
                            xE <- fst goEast.[0]
                            yE <- snd goEast.[0]
                            if  additional_visited |> List.contains (xE,yE)
                            then E <- true else 
                            distanceE <- distanceE + 1
                        | _ ->
                            E <- true
                            yield {area = Space; distance = distanceE; position = (xE, yE)}
                    | Wall -> 
                        E <- true
                    | area ->
                        E <- true
                        yield {area = area; distance = distanceE; position = (xE, yE)}
            
        |]
    {branches = branches; visited = additional_visited}

let findBranches (map: char [] []) (visited: (int * int) list) (position: int * int) 
    : Explore =
    findBranchesGo position (getRect map) visited map

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
    let newbranches, back, keys =
        match step with
        | SpaceStep ->
            otherBranches i solution, Some  branches.[i], solution.keys
        | KeyStep ->
            otherBranches i solution 
            |> Array.append branches.[i].branches, None, area :: solution.keys
            
    {keys=keys; tree = {area = Space; distance = distance; branches = newbranches; back = back} }

let rec findSolution (solution: Solution) : Solution option =
    let branches = solution.tree.branches
    if  (branches = [||] ) then Some solution else
    let alternatives : Solution [] =
        [|
            for i in 0..branches.Length-1 do
                if branches.[i].area = '#' then () else
                if branches.[i].area = Space then
                    let solution = next SpaceStep i solution
                    match findSolution solution with
                    | Some solution -> yield solution
                    | None -> ()
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
    printfn "%A" map
    let start = findStart map
    map.[snd start].[fst start] <- Space
    let tree = buildTree map [start] {area=Space; distance=0; position = start};
    printfn "tree %A" tree
    let solution = findSolution {keys = []; tree = tree}
    printfn "solution %A" solution
    printfn "executed in %d ms"  sw.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    