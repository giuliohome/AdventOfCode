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

type Branch = {area: char; distance: int; position: (int * int)}
type Explore = {branches: Branch[]; visited: (int * int) []}

let findBranches (map: char [] []) (visited: (int * int)[]) (position: int * int) 
    : Explore =
    {branches = [||]; visited = [||]}

let rec buildTree (map: char [] []) (visited: (int * int)[]) (branch: Branch) : Tree =
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

let space = '.'
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

    let map = parse @"C:\dev\FSharp\AoC2019\Day18\input.txt"

    printfn "executed in %d ms"  sw.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    