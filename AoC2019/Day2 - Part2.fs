open System
open System.IO
open System.Diagnostics

let readInts (path:string) : int [] =
    use sw = new StreamReader (path)
    sw.ReadToEnd().Split(",")
    |> Array.map Int32.Parse

type Status = {memory:int[]; position: int; finished: bool;}
type Cmd = {opcode: int; op1:int ; op2: int; dest:int}

let runCmd (cmd:Cmd) (before: Status) : Status =
    let updated = 
        before.memory
        |> Array.mapi(fun pos value ->
            if pos = cmd.dest then
                (match cmd.opcode with
                | 1 -> (+)
                | 2 -> (*)
                | _ -> failwith "wrong position")
                    before.memory.[cmd.op1]
                    before.memory.[cmd.op2]
            else value
        )
    {memory = updated; position = before.position + 4; finished = updated.[before.position + 4] = 99}

let scenario (input: int[]) (noun:int) (verb:int) =
    let resetMemory =
        input
        |> Array.mapi (fun pos value ->
            match pos with
            | 1 -> noun
            | 2 -> verb
            | _ -> value
        )
    let start = {memory = resetMemory; position = 0; finished = false}
    Seq.unfold 
        (fun state ->
            match state.finished with
            | true -> None
            | false -> 
                let cmd = 
                    {
                        opcode = state.memory.[state.position]
                        op1 =  state.memory.[state.position + 1] 
                        op2 =  state.memory.[state.position + 2] 
                        dest = state.memory.[state.position + 3] 
                    }
                let updated = runCmd cmd state
                Some (updated.memory.[0], updated)
        ) start
    |> Seq.last

    

[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()
    let input = readInts @"C:\dev\FSharp\AdventOfCode\Day2\input.txt"
    //let noun = 12 
    //let  verb = 2
    for noun in [0..99] do
        for verb in [0..99] do
            let result = scenario input noun verb
            if result = 19690720 then
                //result.memory
                //|> Array.iter (printfn "%d")
                printfn "The answer is: %d" (100 * noun + verb) // 5936
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    
