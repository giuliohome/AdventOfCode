namespace AoC2019

module IntCode =
    open System
    open System.IO


    let mem_size = 10000

    let readInts (path:string) : int64 [] =
        use sw = new StreamReader (path)
        sw.ReadToEnd().Split(',')
        |> Array.map Int64.Parse

    type Status = 
        { 
            position: int; finished: bool; mutable suspended: bool; 
            mutable output: int64[]; mutable phase: int64[]; base_addr: int}
    type Destinations = |Immediate of int64| MemoryDest of int | Skip

    let getParamVal (mode:int64) (memory: byref<int64[]>) (pos:int) (base_addr:int) =
        match mode with
        | 0L -> memory.[ (int)memory.[pos] ]
        | 1L -> memory.[pos]
        | 2L -> memory.[base_addr + (int)memory.[pos] ]
        | _ -> failwith "wrong par mode"

    let runCmd (before: Status) (memory:byref<int64[]>) : Status =
        let opcodeStr = memory.[before.position]
        let  opcode = opcodeStr % 100L
        let mode_op3 = opcodeStr / 10000L
        if mode_op3 = 1L then failwith "dest must be pos or rel mode"
        let mode_op2 =((opcodeStr / 100L) % 100L) / 10L
        let mode_op1 =((opcodeStr / 100L) % 100L) % 10L
        let dest =
            match opcode with
            | 1L 
            | 2L 
            | 7L
            | 8L -> MemoryDest ((int)memory.[before.position + 3] + (if mode_op3 = 2L then before.base_addr else 0))
            | 3L -> MemoryDest ((int)memory.[before.position + 1] + (if mode_op1 = 2L then before.base_addr else 0))
            | 4L 
            | 5L
            | 6L 
            | 9L -> Skip
            | _ -> failwith "wrong position"
        
        match dest with
        | Immediate _ -> failwith "dest can't be immediate"
        | MemoryDest dest ->
            if dest >= mem_size then failwith "please extend memory"
            memory.[dest] <-
                    match opcode with
                    | 1L -> 
                        (+)
                            (getParamVal mode_op1 &memory (before.position + 1) before.base_addr)
                            (getParamVal mode_op2 &memory (before.position + 2) before.base_addr)
                    | 2L -> 
                        (*)
                            (getParamVal mode_op1 &memory (before.position + 1) before.base_addr)
                            (getParamVal mode_op2 &memory (before.position + 2) before.base_addr)
                    | 3L -> (int64) (Array.head before.phase)
                    | 7L -> if ((getParamVal mode_op1 &memory (before.position + 1) before.base_addr) 
                                  < (getParamVal mode_op2 &memory (before.position + 2) before.base_addr)
                                ) then 1L else 0L
                    | 8L -> if ((getParamVal mode_op1 &memory (before.position + 1) before.base_addr) 
                                   = (getParamVal mode_op2 &memory (before.position + 2) before.base_addr)
                                ) then 1L else 0L
                    | _ -> failwith "wrong position"
        | Skip -> ()
        let output =
            match opcode with
            | 4L ->
                [|(getParamVal mode_op1 &memory (before.position + 1) before.base_addr)|]
                |> Array.append before.output
            | _ -> before.output
        let position =
            match opcode with
            | 1L
            | 2L 
            | 7L
            | 8L -> before.position + 4
            | 3L
            | 4L 
            | 9L -> before.position + 2
            | 5L -> if ((getParamVal mode_op1 &memory (before.position + 1) before.base_addr) <> (int64) 0) 
                    then (int)(getParamVal mode_op2 &memory (before.position + 2) before.base_addr) 
                    else before.position + 3
            | 6L -> if ((getParamVal mode_op1 &memory (before.position + 1) before.base_addr) = (int64)  0) 
                    then (int)(getParamVal mode_op2 &memory (before.position + 2) before.base_addr) 
                    else before.position + 3
            | _  -> failwith "wrong position"
        let phase = 
            if opcode =  3L then Array.tail before.phase else before.phase
        let base_addr = 
            if opcode = 9L 
            then before.base_addr + (int)(getParamVal mode_op1 &memory (before.position + 1) before.base_addr)
            else before.base_addr

        let opcodeStrNext = memory.[position]
        let  opcodeNext = opcodeStrNext % 100L

        { 
            position = position; finished = opcodeNext = 99L; 
            suspended = (opcodeNext = 3L) && (phase.Length = 0); output = output; phase = phase; base_addr = base_addr}

    let extendMemory (memory:Int64[]) (initial:int64 array) : int64[] * Status = 
        let extended_memory = 
            [|0 .. mem_size - 1|]
            |> Array.mapi(fun i t ->
                if i < memory.Length 
                then memory.[i]
                else (int64)0
            )
        extended_memory, {position = 0; finished = false; suspended = false; output = [||]; phase = initial; base_addr= 0}

    let bootstrap (input_path:string) (initial:int64 array) : int64[] * Status =
        let memory = readInts input_path
        extendMemory memory initial 
    