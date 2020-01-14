open System
open System.IO
open Checked
open System.Diagnostics
//https://adventofcode.com/2019/day/18
open MyQueue

let parse (path:string) =
    [|  
        use sr = new StreamReader(path)
        while (not sr.EndOfStream) do
            yield sr.ReadLine().ToCharArray()
    |]

[<Literal>]
let Space = '.'
[<Literal>]
let Wall = '#'
[<Literal>]
let Entrance = '@'

type Tree = {area:char; distance: int; branches: Tree[]; back: Tree option}

type Grid = {distance: int; needed: char list}

let isKey (c:char) : bool =
     c >= 'a' && c <= 'z'

let rec tree2grid (trees:Tree[]) (grid:byref<Map<char, Grid>>) (distance:int) (needed: char list) =
    match trees with
    | [||] -> ()
    | branches ->
        for branch in branches do
            let distance = branch.distance + distance
            let isKey = isKey branch.area
            if isKey then 
                grid <- Map.add branch.area 
                    {
                        distance = distance
                        needed = needed
                    } grid
            tree2grid branch.branches &grid distance 
                (if isKey then 
                    let key = branch.area |> Char.ToLower
                    ( key :: needed)
                else needed)

let grid2branches (having_keys: char list) (grid:Map<char, Grid>) : Tree[] =
    grid
    |> Map.filter(fun k g -> g.needed |> List.forall (fun c -> having_keys |> List.contains c))
    |> Map.toArray
    |> Array.map(fun (k,g) -> {area=k; distance = g.distance; back = None; branches = [||]})

let grid2tree (here:char) (distance:int) (having_keys: char list) (grid:Map<char, Grid>) : Tree =
    match here with
    | Entrance -> 
        let branches = grid2branches having_keys grid
        {area = here; distance=distance; branches=branches; back = None} 
    | key when isKey key -> failwith "TODO - wip"
    | _ -> failwith "only keys or entrance"

let countKeys (map: char [] []) =
    map
    |> Array.sumBy(fun line ->
        line |> Array.filter(fun c -> c >= 'a' && c <= 'z') |> Array.length
    )
let getKeys (map: char [] []) =
    map
    |> Array.collect(fun line ->
        line |> Array.filter(fun c -> c >= 'a' && c <= 'z') )


let findChar c (map: char [] []) =
    let i2 =
        map
        |> Array.findIndex(Array.contains c)
    let i1 = 
        map.[i2] 
        |> Array.findIndex ((=) c)
    (i1,i2)
let findStart =
    findChar Entrance

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

type Solution = {keys: char list; tree: Tree}

let rec pruneSpace (branches: Tree[]): Tree[] =
    [|
        for branch in branches do
            if branch.area <> Space then
                yield branch
            else if branch.branches.Length = 1 then
                yield {branch.branches.[0] with distance = branch.branches.[0].distance + branch.distance}
            else 
                yield branch
    |] 

let rec prune (keys: char list) (branches: Tree[]) : Tree[] =
    let opened =
        keys |> List.map (fun k -> Char.ToUpper k)
    let pruned =
        [|    
            for branch in branches do
                if opened |> List.contains branch.area // && branch.branches.Length = 1 
                then
                    let newback =
                        Option.map 
                            (fun (b : Tree) -> {b with distance = b.distance + branch.distance})
                            branch.branches.[0].back
                    let partial =
                        (branch.branches)
                        |> Array.map(fun b ->
                            {b with distance = branch.branches.[0].distance + branch.distance; back = newback})
                    yield! prune keys partial
                else 
                    yield branch
            |]
    pruneSpace pruned

let otherBranches (i:int) (solution: Solution) : Tree =
    let branches = solution.tree.branches
    let others = 
        [|0..branches.Length-1|]
        |> Array.except [|i|]
        |> Array.map(fun other ->
            branches.[other] //{branches.[other] with distance = branches.[other].distance +  branches.[i].distance}
        )
        |> Array.append
            (Option.fold 
                (fun _ backtree -> 
                    [| backtree |] // {backtree with distance = backtree.distance + branches.[i].distance}|]
                ) 
                [||] solution.tree.back)
    if others.Length = 1 then {others.[0] with distance = others.[0].distance + branches.[i].distance} 
    else
    {area = Space; back = None; branches = others; distance = branches.[i].distance}

type Step = | KeyStep | SpaceStep

let next (step:Step) (i:int) (solution: Solution) grid : Solution =
    let branches = solution.tree.branches
    let distance = solution.tree.distance + branches.[i].distance
    let area = branches.[i].area  
    //let newbranches, back, keys =
    match step with
    | SpaceStep ->
        failwith "not expected with smart grid"
    | KeyStep ->
        let keys = area :: solution.keys
        let tree = grid2tree area distance keys grid
        {keys=keys; tree=tree}


let updateMin (mindistance:byref<int option>) (alternatives:byref<Solution list>) (solution:Solution) =
    match mindistance with
    | Some d ->
        if  solution.tree.distance < d 
        then alternatives <- solution :: alternatives
        mindistance <- Some <| min d solution.tree.distance
        printfn "distance at %d" mindistance.Value
                
    | None ->
        alternatives <- solution :: alternatives
        mindistance <- Some solution.tree.distance
        printfn "distance at %d" mindistance.Value

let findSolution (keynum:int) (solution: Solution) grid : Solution option =
    let mutable solution_queue : queue<Solution> = MyQueue.empty
    solution_queue <- enqueue solution_queue solution
    let mutable mindistance : int option = None
    let mutable alternatives : Solution list = List.empty

    while (MyQueue.length solution_queue > 0) do
        let solution = dequeue &solution_queue
        let branches = prune solution.keys solution.tree.branches
        let solution = {solution with tree = {solution.tree with branches = branches}}
        if  (branches = [||] ) then 
            if solution.keys.Length = keynum 
            then updateMin &mindistance &alternatives solution
        else
        match mindistance with
        | Some d when d < solution.tree.distance + (solution.tree.branches |> Array.map (fun t -> t.distance) |> Array.min) -> () 
        | _ ->
        let indexes =
            [|0..branches.Length-1|]
            |> Array.sortBy(fun idx -> ((if isKey branches.[idx].area then 0 else 1) , branches.[idx].distance))
        for i in indexes do
            if branches.[i].area = '#' then 
                //failwith "not expected with smart grid"
                () 
            else
            if branches.[i].area = Space then
                //failwith "not expected with smart grid"
                let solutionNext = next SpaceStep i solution grid
                solution_queue <- enqueue solution_queue solutionNext 
            else
            if (Char.IsLower branches.[i].area) then
                let solutionNext = next KeyStep i solution grid
                if solutionNext.keys.Length = keynum
                then  updateMin &mindistance &alternatives solutionNext
                else
                solution_queue <- enqueue solution_queue solutionNext
            else
            if (Char.IsUpper branches.[i].area) then
                //failwith "not expected with smart grid"
                let needed = Char.ToLower branches.[i].area
                if solution.keys |> List.contains needed |> not then () else
                let solutionNext = next SpaceStep i solution grid
                solution_queue <- enqueue solution_queue solutionNext
  
    match alternatives with
    | [] -> None
    | alternatives ->
        alternatives |> List.minBy(fun a -> a.tree.distance) |> Some

[<EntryPoint>]
let main _ =

    let sw = Stopwatch()
    sw.Start()

    let map = parse @"C:\dev\FSharp\AdventOfCode\Day18\input_demo.txt"
    map
    |> Array.iter(fun l ->
        printfn "%s" (System.String(l))
    )
    let start = findStart map
    let keynum = countKeys map
    map.[snd start].[fst start] <- Space
    let tree = buildTree map [start] {area=Space; distance=0; position = start}
    //printfn "tree %A" tree
    let mutable grid = Map.empty
    tree2grid tree.branches &grid 0 List.empty
    //printfn "smart grid"
    //grid |> Map.iter (fun k t -> printfn "%O %A" k t)
    let fullGrid =
        getKeys map
        |> Array.map(fun k ->
            let kpos = map |> findChar k
            let ktree = buildTree map [kpos] {area=k; distance=0; position = kpos}
            let mutable kgrid = Map.empty
            tree2grid ktree.branches &kgrid 0 List.empty
            (k,kgrid)
        )
        |> Map.ofArray
    //printfn "Full grid ready!\n%A" fullGrid
    let simpletree = grid2tree Entrance 0 [] grid
    printfn "test grid2tree %A" simpletree
    let solution = findSolution keynum {keys = []; tree = simpletree} grid
    match solution with
    | None -> 
        printfn "solution not found"
    | Some found -> 
        printfn "solution %s" (String.Join("-",found.keys |> List.rev))
        solution |> Option.iter (fun s -> printfn "Answer 1 %d" s.tree.distance)
    printfn "executed in %d ms"  sw.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0