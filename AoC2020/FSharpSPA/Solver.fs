namespace FSharpSPA
open WebSharper
[<JavaScript>]
module Solver =
    open System

    let readInts (input:string) : int [] =
        input.Split(',')
        |> Array.map Int32.Parse
    
    type Status = {memory:int[]; position: int; finished: bool}
    type Cmd = {opcode: int; op1:int ; op2: int; dest:int}

    let runCmd (cmd:Cmd) (before: Status) : Status =
        match cmd.opcode with
        | 1 -> 
            let updated = 
                before.memory
                |> Array.mapi(fun pos value ->
                    if pos = cmd.dest then
                        before.memory.[cmd.op1] +
                        before.memory.[cmd.op2]
                    else value
                )
            {memory = updated; position = before.position + 4; finished = updated.[before.position + 4] = 99}
        | 2 -> 
            let updated = 
                before.memory
                |> Array.mapi(fun pos value ->
                    if pos = cmd.dest then
                        before.memory.[cmd.op1] *
                        before.memory.[cmd.op2]
                    else value
                )
            {memory = updated; position = before.position + 4; finished = updated.[before.position + 4] = 99}
        | _ -> 
            failwith "wrong position"
    
    let Solve (input: string) =
        let input = readInts input
        let start = {memory = input; position = 0; finished = false}
        let mutable reset = true
        let firstCmd = {
                            opcode = start.memory.[start.position]
                            op1 =  start.memory.[start.position + 1] 
                            op2 =  start.memory.[start.position + 2] 
                            dest = start.memory.[start.position + 3] 
                        }
        let resetMemory =
            start.memory
            |> Array.mapi (fun pos value ->
                match pos with
                | 1 -> 12
                | 2 -> 2
                | _ -> value
            )
        let mutable ProgramEnded = false
        let result = 
            Seq.unfold 
                (fun state ->
                    match state.finished with
                    | true -> None
                    | false -> 
                        let cmd = 
                            if reset then firstCmd else
                            {
                                opcode = state.memory.[state.position]
                                op1 =  state.memory.[state.position + 1] 
                                op2 =  state.memory.[state.position + 2] 
                                dest = state.memory.[state.position + 3] 
                            }
                        let updated = runCmd cmd (if reset then {state with memory = resetMemory} else state)
                        if reset then reset <- false
                        Some (updated, updated)
                ) start
            |> Seq.last
        result.memory