open System
open System.IO
open System.Diagnostics

let readInts (path:string) : int [] =
    use sw = new StreamReader (path)
    sw.ReadToEnd().Split(",")
    |> Array.map Int32.Parse

type Status = {memory:int[]; position: int; finished: bool; output: int list}
type Destinations = |Immediate of int | MemoryDest of int | OutputDest
let input = 1

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
        | 2 -> MemoryDest before.memory.[before.position + 3]
        | 3 -> MemoryDest before.memory.[before.position + 1]
        | 4 -> OutputDest
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
                    | 3 -> input
                    | _ -> failwith "wrong position"
                else value
            )
        | OutputDest -> before.memory
    let output =
        match opcode with
        | 4 ->
            (getParamVal mode_op1 before.memory (before.position + 1))
            :: before.output
        | _ -> before.output
    let next =
        match opcode with
        | 1
        | 2 -> 4
        | 3
        | 4 -> 2
        | _  -> failwith "wrong position"
    {memory = updated; position = before.position + next; finished = updated.[before.position + next] = 99; output = output}

let scenario (input: int[]) (noun:int) (verb:int) =
    //let resetMemory =
    //    input
    //    |> Array.mapi (fun pos value ->
    //        match pos with
    //        | 1 -> noun
    //        | 2 -> verb
    //        | _ -> value
    //    )
    let start = {memory = input; position = 0; finished = false; output = []}
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
    let input = readInts @"C:\dev\FSharp\AoC2019\Day5\input.txt"
    let noun = 12 
    let  verb = 2
    let output = scenario input noun verb |> List.rev
    printfn "Diagnostic codes:"
    output |> List.iter(printfn "%d")
    let result = match output with
    | [] -> "no output"
    | output -> sprintf "final output: %d" (output |> List.last)
    printfn "The answer is: %s" result
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    
