open System
open System.IO

// copied from my previous Day9 - Part2.fs but suspension modified for wait on input  START

let mem_size = 10000

let readInts (path:string) : int64 [] =
    use sw = new StreamReader (path)
    sw.ReadToEnd().Split(",")
    |> Array.map Int64.Parse

type Status = 
    { 
        memory:int64[]; position: int; finished: bool; suspended: bool; 
        output: int64 list; phase: int[]; base_addr: int}
type Destinations = |Immediate of int64| MemoryDest of int | Skip

let getParamVal (mode:int) (memory: int64[]) (pos:int) (base_addr:int) =
    match mode with
    | 0 -> memory.[ (int)memory.[pos] ]
    | 1 -> memory.[pos]
    | 2 -> memory.[base_addr + (int)memory.[pos] ]
    | _ -> failwith "wrong par mode"

let runCmd (before: Status) : Status =
    let opcodeStr = before.memory.[before.position].ToString("00000")
    let  opcode = int (opcodeStr.Substring(3,2))
    let mode_op3 = int (opcodeStr.Substring(0,1))
    if mode_op3 = 1 then failwith "dest must be pos or rel mode"
    let mode_op2 = int (opcodeStr.Substring(1,1))
    let mode_op1 = int (opcodeStr.Substring(2,1))
    let dest =
        match opcode with
        | 1 
        | 2 
        | 7
        | 8 -> MemoryDest ((int)before.memory.[before.position + 3] + (if mode_op3 = 2 then before.base_addr else 0))
        | 3 -> MemoryDest ((int)before.memory.[before.position + 1] + (if mode_op1 = 2 then before.base_addr else 0))
        | 4 
        | 5
        | 6 
        | 9 -> Skip
        | _ -> failwith "wrong position"
    let updated = 
        match dest with
        | Immediate _ -> failwith "dest can't be immediate"
        | MemoryDest dest ->
            if dest >= mem_size then failwith "please extend memory"
            before.memory
            |> Array.mapi(fun pos value ->
                if pos = dest then
                    match opcode with
                    | 1 -> 
                        (+)
                            (getParamVal mode_op1 before.memory (before.position + 1) before.base_addr)
                            (getParamVal mode_op2 before.memory (before.position + 2) before.base_addr)
                    | 2 -> 
                        (*)
                            (getParamVal mode_op1 before.memory (before.position + 1) before.base_addr)
                            (getParamVal mode_op2 before.memory (before.position + 2) before.base_addr)
                    | 3 -> (int64) (Array.head before.phase)
                    | 7 -> if ((getParamVal mode_op1 before.memory (before.position + 1) before.base_addr) 
                                < (getParamVal mode_op2 before.memory (before.position + 2) before.base_addr)
                           ) then (int64)1 else (int64)0
                    | 8 -> if ((getParamVal mode_op1 before.memory (before.position + 1) before.base_addr) 
                                = (getParamVal mode_op2 before.memory (before.position + 2) before.base_addr)
                           ) then (int64)1 else (int64)0
                    | _ -> failwith "wrong position"
                else value
            )
        | Skip -> before.memory
    let output =
        match opcode with
        | 4 ->
            (getParamVal mode_op1 before.memory (before.position + 1) before.base_addr)
            :: before.output |> List.rev
        | _ -> before.output
    let position =
        match opcode with
        | 1
        | 2 
        | 7
        | 8 -> before.position + 4
        | 3
        | 4 
        | 9 -> before.position + 2
        | 5 -> if ((getParamVal mode_op1 before.memory (before.position + 1) before.base_addr) <> (int64) 0) 
               then (int)(getParamVal mode_op2 before.memory (before.position + 2) before.base_addr) 
               else before.position + 3
        | 6 -> if ((getParamVal mode_op1 before.memory (before.position + 1) before.base_addr) = (int64)  0) 
               then (int)(getParamVal mode_op2 before.memory (before.position + 2) before.base_addr) 
               else before.position + 3
        | _  -> failwith "wrong position"
    let phase = 
        if opcode =  3 then Array.tail before.phase else before.phase
    let base_addr = 
        if opcode = 9 
        then before.base_addr + (int)(getParamVal mode_op1 before.memory (before.position + 1) before.base_addr)
        else before.base_addr

    let opcodeStrNext = updated.[position].ToString("00000")
    let  opcodeNext = int (opcodeStrNext.Substring(3,2))

    { 
        memory = updated; position = position; finished = opcodeNext = 99; 
        suspended = (opcodeNext = 3) && (phase.Length = 0); output = output; phase = phase; base_addr = base_addr}

// copied from Day9 - Part2.fs but suspension modified for wait on input END 

let stop_condition (state:Status) = 
    state.finished || state.suspended
let scenario (start:Status) : Status =
    if stop_condition start then start else
    Seq.unfold 
        (fun state ->
            match stop_condition state with
            | true -> None
            | false -> 
                let updated = runCmd state
                Some (updated, updated)
        ) start
    |> Seq.last

let bootstrap (input_path:string) (initial:int array) : Status =
    let memory = readInts input_path
    let extended_memory = 
        [|0 .. mem_size - 1|]
        |> Array.mapi(fun i t ->
            if i < memory.Length 
            then memory.[i]
            else (int64)0
        )
    {memory = extended_memory; position = 0; finished = false; suspended = false; output = []; phase = initial; base_addr= 0}
    


type RoboColor = 
    | PaintBlack 
    | PaintWhite
    member this.toInt () =
        match this with
        | PaintBlack -> 0
        | PaintWhite -> 1
    static member Parse = function
        | 0 -> PaintBlack
        | 1 -> PaintWhite
        | _ -> failwith "wrong color"

type RoboMove = 
    | TurnLeft 
    | TurnRight
    static member Parse = function
        | 0 -> TurnLeft
        | 1 -> TurnRight
        | _ -> failwith "wrong move"

type RoboDir = 
    | FaceUp | FaceRight | FaceDown | FaceLeft

let turnFace (move:RoboMove) = function
    | FaceUp -> 
        match move with
        | TurnLeft -> FaceLeft
        | TurnRight -> FaceRight
    | FaceRight ->
        match move with
        | TurnLeft -> FaceUp
        | TurnRight -> FaceDown
    | FaceDown ->
        match move with
        | TurnLeft -> FaceRight
        | TurnRight -> FaceLeft
    | FaceLeft ->
        match move with
        | TurnLeft -> FaceDown
        | TurnRight -> FaceUp

type Instruction = {color:RoboColor; move:RoboMove}

type PanelPosition = {X:int; Y:int; Dir:RoboDir}
    with
    member this.toCoord() = (this.X, this.Y)
    member this.run (instr:Instruction) =
        let nextDir = turnFace instr.move this.Dir
        match nextDir with
        | FaceUp -> 
            {Dir = nextDir; X = this.X; Y = this.Y + 1}
        | FaceRight -> 
            {Dir = nextDir; X = this.X + 1; Y = this.Y}
        | FaceDown -> 
            {Dir = nextDir; X = this.X; Y = this.Y - 1}
        | FaceLeft -> 
            {Dir = nextDir; X = this.X - 1; Y = this.Y}

type PanelsArea = 
    {map:Map<int * int, RoboColor>; curr_pos: PanelPosition }
    with
    member this.run (instr:Instruction) =
        //printfn "running instruction %A" instr
        let nextMap =
            this.map
            |> Map.remove (this.curr_pos.toCoord())
            |> Map.add (this.curr_pos.toCoord()) instr.color
        let nextPos = this.curr_pos.run instr
        {map = nextMap; curr_pos = nextPos}
    member this.PaintPanel (x:int) (y:int) : char =
        if this.map.ContainsKey((x,y)) 
        then 
            match this.map.[(x,y)] with
            | PaintWhite -> 'X'
            | PaintBlack -> ' '
        else ' '
    member this.PaintArea() = 
        let minX = 
            this.map |> Map.toArray |> Array.map fst |> Array.map fst |> Array.min
        let maxX = 
            this.map |> Map.toArray |> Array.map fst |> Array.map fst |> Array.max
        let minY = 
            this.map |> Map.toArray |> Array.map fst |> Array.map snd |> Array.min
        let maxY =
            this.map |> Map.toArray |> Array.map fst |> Array.map snd |> Array.max
        //printfn "map of x from %d to %d - y from %d to %d area = %d" minX maxX minY maxY
        //    ([minX..maxX].Length * [minY..maxY].Length)
        [minY .. maxY]
        |> List.rev
        |> List.iter(fun y ->
            [minX .. maxX]
            |> List.iter (fun x ->
                let here = (this.curr_pos.X = x && this.curr_pos.Y = y)
                printf "%c" (if here then '*'  else (this.PaintPanel x y))
            )
            printfn ""
        )
        printfn ""

let solver =
            fun (area, state) -> 
            if state.finished then None else
            let camera =
                if  (area.map.ContainsKey (area.curr_pos.toCoord()))
                then area.map.[area.curr_pos.toCoord()]
                else PaintBlack
            let nextState = scenario {state with phase = [| camera.toInt()|]} 
            if nextState.output.Length <> 2 then failwith "wrong output" else
            if nextState.phase.Length > 0 then failwith "wrong phase" else 
            let instr = 
                { 
                    color = RoboColor.Parse ((int)nextState.output.[0])
                    move = RoboMove.Parse ((int)nextState.output.[1])
                }
            let next_area = area.run instr 
            //printfn "current count = %d and dir %A" next_area.map.Count next_area.curr_pos.Dir
            //next_area.PaintArea()
            //Console.ReadKey() |> ignore
            Some (next_area, (next_area, {nextState with suspended = false; output = []}))

[<EntryPoint>]
let main _ =
    // see https://adventofcode.com/2019/day/11
    let startPos =
        {X = 0; Y = 0; Dir = FaceUp}
    let startMap = [|((0,0), PaintBlack)|] |> Map.ofArray
    let startArea = {map = startMap; curr_pos = startPos}
    let startStatus = bootstrap  @"C:\dev\FSharp\AdventOfCode\Day11\input.txt" [||]
    let endArea =
        Seq.unfold
            solver
            (startArea, startStatus)
        |> Seq.last
    printfn "Answer Part 1 is %d" endArea.map.Count
    let startPos =
        {X = 0; Y = 0; Dir = FaceUp}
    let startMap = [|((0,0), PaintWhite)|] |> Map.ofArray
    let startArea = {map = startMap; curr_pos = startPos}
    let startStatus = bootstrap  @"C:\dev\FSharp\AdventOfCode\Day11\input.txt" [||]
    let endArea =
        Seq.unfold
            solver
            (startArea, startStatus)
        |> Seq.last
    printfn "Answer Part 2 is" 
    endArea.PaintArea()

    Console.ReadKey() |> ignore
    0    
