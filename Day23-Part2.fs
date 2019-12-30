open System
open System.IO
open Checked
open System.Diagnostics
open AoC2019.IntCode


let parse (str:string) : int[] =
    str.ToCharArray()
    |> Array.map(fun c -> int <| string c)

//https://adventofcode.com/2019/day/23

type IdleStatus = {queueEmpty: bool; tryReceive: int; noOutput: bool}
    

[<EntryPoint>]
let main _ =

    let sw = Stopwatch()
    sw.Start()
    
    let mutable network = 
        [|
            for address in 0L..49L do
                yield bootstrap "C:\dev\FSharp\AoC2019\Day23\input.txt" [|address|]
        |]

    let mutable stop = false
    let mutable found : int64 Option = None
    let net_input : int64[][] = Array.create 50 [||]
    let mutable natX : int64 option = None
    let mutable natY : int64 option = None
    let idles : IdleStatus[] = Array.create 50 {noOutput=false; tryReceive=0; queueEmpty = false}
    let mutable network_idle = false

    while not stop do
        if network.Length <> 50 then failwith "network lost"
        network <- 
            [|
                for address in 0..49 do
                    if network.[address].finished 
                    then 
                        idles.[address] <- {idles.[address] with tryReceive= idles.[address].tryReceive + 1 ; queueEmpty = true; noOutput = true}
                        yield network.[address] 
                    else 
                        if  network.[address].suspended 
                        then 
                            let phase = 
                                if net_input.[address] = [||] 
                                then
                                    idles.[address] <- 
                                        {idles.[address] with tryReceive= idles.[address].tryReceive + 1 ; queueEmpty = true}
                                    match natX, natY, network_idle, address with
                                    | Some x, Some y, true, 0 -> 
                                        Debug.WriteLine(sprintf "Nat to address 0 %d" y)
                                        idles.[address] <- {idles.[address] with tryReceive=0; queueEmpty = false}
                                        network_idle <- false
                                        if found = Some y then
                                            Debug.WriteLine(sprintf "Found ! %d" y)
                                            stop <- true
                                        else 
                                            found <- Some y
                                        [| x; y|]
                                    | _ -> 
                                        [|-1L|]
                                     
                                else
                                    idles.[address] <-  {idles.[address] with tryReceive=0; queueEmpty = false}
                                    Debug.WriteLine(sprintf "%d received %A input" address net_input.[address]) 
                                    net_input.[address]
                            yield {network.[address] with phase = phase; suspended = false }
                            net_input.[address] <- [||]
                        else 
                            if network.[address].phase.Length = 0 then
                                idles.[address] <-  {idles.[address] with queueEmpty = true}
                            yield network.[address] 
            |]
        if network.Length <> 50 then failwith "network lost"
        network <- 
            [|
                for address in 0..49 do
                    if network.[address].finished 
                    then yield network.[address] 
                    else 
                    if network.[address].output.Length > 3 then failwith "output overflow" 
                    if network.[address].output.Length < 3 
                    then
                        //Debug.WriteLine(sprintf "Running %d" address) 
                        idles.[address] <-  {idles.[address] with noOutput = true}
                        yield runCmd network.[address] 
                    else
                        idles.[address] <-  {idles.[address] with noOutput = false}
                        if network.[address].output.[0] = 255L 
                        then
                            Debug.WriteLine(sprintf "Nat from %d receives Y %d" address network.[address].output.[2])
                            natY <- Some network.[address].output.[2]
                            natX <- Some network.[address].output.[1]
                            yield runCmd {network.[address] with output = [||] }
                        else 
                            Debug.WriteLine(sprintf "%d %d arrived to %d having %d" 
                                network.[address].output.[1] network.[address].output.[2] network.[address].output.[0]
                                net_input.[(int)network.[address].output.[0]].Length
                                )
                            net_input.[(int)network.[address].output.[0]] <-
                                Array.append
                                    net_input.[(int)network.[address].output.[0]]
                                    [|network.[address].output.[1]; network.[address].output.[2]|]
                            
                            Debug.WriteLine(sprintf "Running %d after output %A" address network.[address].output) 
                            Debug.WriteLine(sprintf "Queue %d is %A" network.[address].output.[0] net_input.[(int)network.[address].output.[0]]) 
                            yield runCmd {network.[address] with output = [||] }
                        

            |]
        if network.Length <> 50 then failwith "network lost"
        network_idle <-
            idles
            |> Array.forall(fun idle -> idle.noOutput && idle.queueEmpty && idle.tryReceive >= 10)
        if network_idle then Debug.WriteLine("network idle!")

    sw.Stop()

    printfn "Answer Part 2 is %A in %d ms" found sw.ElapsedMilliseconds
    
    Console.ReadKey() |> ignore
    0    