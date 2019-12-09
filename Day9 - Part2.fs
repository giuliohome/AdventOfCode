open System
open System.IO
open System.Diagnostics

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
            :: before.output
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
    { 
        memory = updated; position = position; finished = updated.[position] = (int64)99; 
        suspended = opcode = 4; output = output; phase = phase; base_addr = base_addr}

let scenario (start:Status) =
    Seq.unfold 
        (fun state ->
            match state.finished with
            | true -> None
            | false -> 
                let updated = runCmd state
                Some (updated.output, updated)
        ) start
    |> Seq.last



[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()
    let memory = readInts @"C:\dev\FSharp\AoC2019\Day9\input.txt"
    let extended_memory = 
        [|0 .. mem_size - 1|]
        |> Array.mapi(fun i t ->
            if i < memory.Length 
            then memory.[i]
            else (int64)0
        )
    let init = {memory = extended_memory; position = 0; finished = false; suspended = false; output = []; phase = [|2|]; base_addr= 0}
    let result = scenario init |> List.rev

    printfn "The answer is %s" (String.Join(",",result))

    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    