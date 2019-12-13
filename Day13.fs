// F# solution for
// https://adventofcode.com/2019/day/13

open System
open System.IO
// .NET uses unchecked arithmetic by default. 
// Open the 'Checked' module in F# to shadow the usual unchecked operators with checked ones, and overflows will cause an exception.
open Checked // https://twitter.com/oratnac/status/1205484377444167680
open System.Diagnostics

// copied from my previous Day11.fs but List.append for output - START

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
            [(getParamVal mode_op1 before.memory (before.position + 1) before.base_addr)]
            |> List.append before.output
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

let extendMemory (memory:Int64[]) (initial:int array) (quarters:int option) : Status = 
    let extended_memory = 
        [|0 .. mem_size - 1|]
        |> Array.mapi(fun i t ->
            if i = 0 then
                match quarters with
                | None -> memory.[0]
                | Some q -> (int64)q
            else
            if i < memory.Length 
            then memory.[i]
            else (int64)0
        )
    {memory = extended_memory; position = 0; finished = false; suspended = false; output = []; phase = initial; base_addr= 0}

let bootstrap (input_path:string) (initial:int array) (quarters:int option) : Status =
    let memory = readInts input_path
    extendMemory memory initial quarters 
    
// copied from my previous Day11.fs but List.append for output - END

type TileInstr = {x: int; y: int; id: int} with
    static member Parse (out: int64 list) = 
        if out.Length <> 3 then failwith "wrong instruction length" else
        {x = (int)out.[0]; y = (int)out.[1]; id = (int)out.[2]}
    static member IsBlock (instr:TileInstr) =
        instr.id = 2 // 2 is a block tile. Blocks can be broken by the ball.
    static member IsScore (instr:TileInstr) =
    //When three output instructions specify X=-1, Y=0, the third output instruction is not a tile; 
    //the value instead specifies the new score to show in the segment display. 
    //For example, a sequence of output values like -1,0,12345 would show 12345 as the player's current score.        
        instr.x = -1 && instr.y = 0 
    static member IsPaddle (instr:TileInstr) =
    //3 is a horizontal paddle tile. The paddle is indestructible.
        instr.id = 3
    static member IsBall (instr:TileInstr) =
    //4 is a ball tile. The ball moves diagonally and bounces off objects.
        instr.id = 4
    static member IsWall (instr:TileInstr) =
    //1 is a wall tile. Walls are indestructible barriers
        instr.id = 1

type ScoreStatus = {score: int; status: Status; round: int; ballMaybe: TileInstr option; paddleMaybe: TileInstr option}

let reply (instr: TileInstr list) (score:int)  
    (ballMaybe: TileInstr option) (paddleMaybe: TileInstr option)
    : (int[] * int * TileInstr option * TileInstr option) =
    let score = 
        instr
        |> List.tryFindBack TileInstr.IsScore
        |> Option.map(fun instr -> instr.id)
        |> Option.fold (fun _ x -> x) score 
    //let walls =
    //    instr
    //    |> List.filter TileInstr.IsWall
    // x range 0 - 44
    //walls
    //|> List.iter (fun i -> 
    //    printfn "Wall %d %d" i.x i.y)
    let paddleMaybe =
        instr
        |> List.tryFindBack TileInstr.IsPaddle
        |> Option.fold (fun _ x -> Some x) paddleMaybe
    let ballNowMaybe =
        instr
        |> List.tryFindBack TileInstr.IsBall
    let ballNow =
        match ballNowMaybe, ballMaybe with
        | Some ballNow, _ -> ballNow
        | _, ballMaybe -> ballMaybe.Value
    let ball =
        match ballMaybe with
        | None -> ballNow
        | Some ballBefore ->
            //printfn "before %d %d now %d %d" ballBefore.x ballBefore.y ballNow.x ballNow.y
            {ballNow with 
                x = ballNow.x + 
                    match paddleMaybe with
                    | Some paddle ->
                        (   if ballNow.y < paddle.y - 1
                            then (ballNow.x - ballBefore.x) 
                            else 0)
                    | None -> (ballNow.x - ballBefore.x)
            }

    //If the joystick is in the neutral position, provide 0.
    //If the joystick is tilted to the left, provide -1.
    //If the joystick is tilted to the right, provide 1.
    let move : int =
        match paddleMaybe with
        | Some paddle ->
            //printfn "paddle %d %d" paddle.x paddle.y
            if ball.x = paddle.x then 0 else 
            if ball.x < paddle.x then -1 else 
            1
        | _ -> 0
    //printfn "move: %d" move
    ([|move|], score, 
        Some ballNow, 
        paddleMaybe
        |> Option.map( fun paddle ->
            {paddle with x = paddle.x + move}))

let playGame (before: ScoreStatus) : ScoreStatus =
    let instructions : TileInstr list=
        before.status.output
        |> List.chunkBySize 3
        |> List.map TileInstr.Parse
    let (input, score, ballMaybe, paddleMaybe) = reply instructions before.score before.ballMaybe before.paddleMaybe
    {
        before 
        with status = {before.status with suspended = false; output = []; phase = input}; 
             score = score; round = before.round + 1;
             ballMaybe = ballMaybe; paddleMaybe = paddleMaybe
    }

let solverPart2 (before: ScoreStatus) 
    : ( ( (int * int) * ScoreStatus) option) =
    if before.status.finished 
    then None 
    else
    let state = scenario before.status
    let next = playGame {before with status = state}
    Some ((next.score, next.round), next)

[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()
    // no input, see clarifications and doubts about the interpretation ==>
    // https://www.reddit.com/r/adventofcode/comments/ea47nz/2019_day_13_part_1_i_dont_understand_what_is/
    let startStatus = bootstrap  @"C:\dev\FSharp\AoC2019\Day13\input.txt" [||] None
    let endStatus = scenario startStatus
    if (not endStatus.finished) 
    then 
        printfn "Warning! Input required!"
        -1
    else
    let instructions : TileInstr list=
        endStatus.output
        |> List.chunkBySize 3
        |> List.map TileInstr.Parse
    let answer1 = 
        instructions 
        |> List.filter TileInstr.IsBlock
        |> List.length
    printfn "The answer for Part 1 is %d" answer1
    // Your puzzle answer was 462.
    // The first half of this puzzle is complete! It provides one gold star: *
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    timer.Restart()
    //Memory address 0 represents the number of quarters that have been inserted; set it to 2 to play for free.
    let startStatus = bootstrap  @"C:\dev\FSharp\AoC2019\Day13\input.txt" [||] (Some 2)
    let (answer2 : int, round2 : int) = 
        Seq.unfold
            solverPart2
            {score = -1; status = startStatus; round = 0; ballMaybe = None; paddleMaybe = None}
        |> Seq.last
    printfn "The answer for Part 2 is %d (after %d rounds)" answer2 round2
    //Your puzzle answer was 23981.
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0 