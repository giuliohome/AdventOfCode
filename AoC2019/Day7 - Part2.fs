open System
open System.IO
open System.Diagnostics

let readInts (path:string) : int [] =
    use sw = new StreamReader (path)
    sw.ReadToEnd().Split(",")
    |> Array.map Int32.Parse

type Status = {memory:int[]; position: int; finished: bool; suspended: bool; output: int list; phase: int[]}
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
    {memory = updated; position = position; finished = updated.[position] = 99; suspended = opcode = 4; output = output; phase = phase}

let scenario (start:Status) =
    Seq.unfold 
        (fun state ->
            match state.finished || state.suspended with
            | true -> None
            | false -> 
                let updated = runCmd state
                Some (updated, updated)
        ) start
    |> Seq.last

    // my first answer 359142 for [3; 4; 1; 2; 0]

    // my second answer is 16 for settings [9; 8; 7; 6; 5] after 9 feedback cycles
    // done in (280) ms !
    

[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()
    let memory = readInts @"C:\dev\FSharp\AoC2019\Day7\input.txt"
    let mutable max_signal = 0
    let mutable settings = [] 
    let mutable feedback = 0
    let mutable output0 = 0
    let mutable output1 = 0
    let mutable output2 = 0
    let mutable output3 = 0
    let mutable output4 = 0
    for i0 in [5..9] do
     for i1 in [5..9] |> List.except [i0] do
      for i2 in [5..9] |> List.except [i0; i1] do
       for i3 in [5..9] |> List.except [i0; i1; i2] do
         for i4 in [5..9] |> List.except [i0; i1; i2; i3] do
            let phase0 = [|i0; 0|]
            let mutable status0 = {memory = memory; position = 0; finished = false; suspended = false; output = []; phase = phase0}
            let phase1 = [|i1|]
            let mutable status1 = {memory = memory; position = 0; finished = false; suspended = false; output = []; phase = phase1}
            let phase2 = [|i2|]
            let mutable status2 = {memory = memory; position = 0; finished = false; suspended = false; output = []; phase = phase2}
            let phase3 = [|i3|]
            let mutable status3 = {memory = memory; position = 0; finished = false; suspended = false; output = []; phase = phase3}
            let phase4 = [|i4|]
            let mutable status4 = {memory = memory; position = 0; finished = false; suspended = false; output = []; phase = phase4}
            output0 <- 0
            output1 <- 0
            output2 <- 0
            output3 <- 0
            output4 <- 0
            feedback <- 0
            while ((not status0.finished) || (not status1.finished) || (not status2.finished) || (not status3.finished) || not status4.finished) do 
                feedback <- feedback + 1
                
                if (not status0.finished) then 
                 status0 <- scenario status0 
                if (status0.output.Length > 0) then 
                    output0 <- status0.output |> List.rev |> List.head
                
                if (status0.output.Length > 0) then
                    status1 <- {status1 with phase = Array.append status1.phase [| output0 |]; suspended = false; output = []}
                else
                    status1 <- {status1 with suspended = false; output = []}
                if (not status1.finished) then 
                   status1 <- scenario status1 
                if (status1.output.Length > 0) then 
                    output1 <- status1.output |> List.rev |> List.head
            
                if (status1.output.Length > 0) then 
                 status2 <- {status2 with phase = Array.append status2.phase [| output1 |]; suspended = false; output = []}
                else
                    status2 <- {status2 with suspended = false; output = []}
                if (not status2.finished) then 
                    status2 <- scenario status2 
                if (status2.output.Length > 0) then 
                    output2 <- status2.output |> List.rev |> List.head
                
                if (status2.output.Length > 0) then 
                    status3 <- {status3 with phase = Array.append status3.phase [| output2 |]; suspended = false; output = []}
                else 
                    status3 <- {status3 with suspended = false; output = []}
                if (not status3.finished) then 
                    status3 <- scenario status3 
                if (status3.output.Length > 0) then 
                    output3 <- status3.output |> List.rev |> List.head
                    
                if (status3.output.Length > 0) then 
                    status4 <- {status4 with phase = Array.append status4.phase [| output3 |]; suspended = false; output = []}
                else 
                    status4 <- {status4 with suspended = false; output = []}
                if (not status4.finished) then 
                    status4 <- scenario status4 
                if (status4.output.Length > 0) then 
                    output4 <- (status4.output |> List.rev |> List.head)
                if (status4.output.Length > 0) then
                    status0 <- {status0 with phase = Array.append status0.phase [| output4 |]; suspended = false; output = []}
                else 
                    status0 <- {status0 with suspended = false; output = []}
            
            
            
            if (output4 > max_signal) then
                max_signal <- output4
                settings <- [i0; i1; i2; i3; i4]
    printfn "The answer is %d for settings %A after %d feedback cycles" max_signal settings feedback
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    
