open System
open System.IO
open Checked
open System.Diagnostics
open AoC2019.IntCode


let parse (str:string) : int[] =
    str.ToCharArray()
    |> Array.map(fun c -> int <| string c)

//https://adventofcode.com/2019/day/23
    

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
    let mutable found : int Option = None
    let net_input : int64[][] = Array.create 50 [||]

    while not stop do
        network <- 
            [|
                for address in 0..49 do
                    if network.[address].finished 
                    then yield network.[address] 
                    else 
                        if  network.[address].suspended 
                        then 
                            let phase = 
                                if net_input.[address] = [||] 
                                then [|-1L|] 
                                else
                                    Debug.WriteLine(sprintf "%d received %A input" address net_input.[address]) 
                                    net_input.[address]
                            yield {network.[address] with phase = phase; suspended = false }
                            net_input.[address] <- [||]
                        else 
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
                        yield runCmd network.[address] 
                    else
                        if network.[address].output.[0] = 255L 
                        then
                            Debug.WriteLine(sprintf "Found from %d" address)
                            found <- Some ((int)network.[address].output.[2])
                            stop <- true
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

    sw.Stop()

    printfn "Answer Part 2 is %A in %d ms" found sw.ElapsedMilliseconds
    
    Console.ReadKey() |> ignore
    0    