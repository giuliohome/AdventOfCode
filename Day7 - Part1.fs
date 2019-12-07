open System
open System.IO
open System.Diagnostics

let readInts (path:string) : int [] =
    use sw = new StreamReader (path)
    sw.ReadToEnd().Split(",")
    |> Array.map Int32.Parse

type Status = {memory:int[]; position: int; finished: bool; output: int list; phase: int[]}
type Destinations = |Immediate of int | MemoryDest of int | Skip

let getParamVal (mode:int) (memory: int[]) (pos:int) =
    match mode with
    | 0 -> memory.[ memory.[pos] ]
    | 1 -> memory.[pos]
    | _ -> failwith "wrong par mode"

let runCmd (before: Status) : Status =
    let opcodeStr = before.memory.[before.position].ToString("00000")
    let  opcode = int (opcodeStr.Substring(3,2))
    let mode_op3 = int (opcodeStr.Substring(0,1))
    if mode_op3 <> 0 then failwith "dest must be pos mode"
    let mode_op2 = int (opcodeStr.Substring(1,1))
    let mode_op1 = int (opcodeStr.Substring(2,1))
    let dest =
        match opcode with
        | 1 
        | 2 
        | 7
        | 8 -> MemoryDest before.memory.[before.position + 3]
        | 3 -> MemoryDest before.memory.[before.position + 1]
        | 4 
        | 5
        | 6 -> Skip
        | _ -> failwith "wrong position"
    let updated = 
        match dest with
        | Immediate _ -> failwith "dest can't be immediate"
        | MemoryDest dest ->
            before.memory
            |> Array.mapi(fun pos value ->
                if pos = dest then
                    match opcode with
                    | 1 -> 
                        (+)
                            (getParamVal mode_op1 before.memory (before.position + 1))
                            (getParamVal mode_op2 before.memory (before.position + 2))
                    | 2 -> 
                        (*)
                            (getParamVal mode_op1 before.memory (before.position + 1))
                            (getParamVal mode_op2 before.memory (before.position + 2))
                    | 3 -> Array.head before.phase
                    | 7 -> if ((getParamVal mode_op1 before.memory (before.position + 1)) 
                                < (getParamVal mode_op2 before.memory (before.position + 2))
                           ) then 1 else 0
                    | 8 -> if ((getParamVal mode_op1 before.memory (before.position + 1)) 
                                = (getParamVal mode_op2 before.memory (before.position + 2))
                           ) then 1 else 0
                    | _ -> failwith "wrong position"
                else value
            )
        | Skip -> before.memory
    let output =
        match opcode with
        | 4 ->
            (getParamVal mode_op1 before.memory (before.position + 1))
            :: before.output
        | _ -> before.output
    let position =
        match opcode with
        | 1
        | 2 
        | 7
        | 8 -> before.position + 4
        | 3
        | 4 -> before.position + 2
        | 5 -> if ((getParamVal mode_op1 before.memory (before.position + 1)) <> 0) 
               then (getParamVal mode_op2 before.memory (before.position + 2)) 
               else before.position + 3
        | 6 -> if ((getParamVal mode_op1 before.memory (before.position + 1)) = 0) 
               then (getParamVal mode_op2 before.memory (before.position + 2)) 
               else before.position + 3
        | _  -> failwith "wrong position"
    let phase = 
        if opcode =  3 then Array.tail before.phase else before.phase
    {memory = updated; position = position; finished = updated.[position] = 99; output = output; phase = phase}

let scenario (memory: int[]) (phase:int[]) =
    let start = {memory = memory; position = 0; finished = false; output = []; phase = phase}
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
    let memory = readInts @"C:\dev\FSharp\AoC2019\Day7\input.txt"
    let mutable max_signal = 0
    let mutable settings = [] 
    for i0 in [0..4] do
        let phase0 = [|i0; 0|]
        let output0 = scenario memory phase0 |> List.rev |> List.head
        for i1 in [0..4] |> List.except [i0] do
            let phase1 = [|i1; output0|]
            let output1 = scenario memory phase1 |> List.rev |> List.head
            for i2 in [0..4] |> List.except [i0; i1] do
                let phase2 = [|i2; output1|] 
                let output2 = scenario memory phase2 |> List.rev |> List.head
                for i3 in [0..4] |> List.except [i0; i1; i2] do
                    let phase3 = [|i3; output2|]
                    let output3 = scenario memory phase3 |> List.rev |> List.head
                    for i4 in [0..4] |> List.except [i0; i1; i2; i3] do
                        let phase4 = [|i4; output3|]
                        let output4 = scenario memory phase4 |> List.rev |> List.head
                        if (output4 >= max_signal) then
                            max_signal <- output4
                            settings <- [i0; i1; i2; i3; i4]
    printfn "The answer is %d for %A" max_signal settings
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    
