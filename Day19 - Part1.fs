open System
open System.IO
open Checked
open System.Diagnostics
//https://adventofcode.com/2019/day/19
open MyQueue
open AoC2019.IntCode


type Position = {X:int;Y:int}
[<EntryPoint>]
let main _ =

    let sw = Stopwatch()
    sw.Start()

    let inputPath = @"C:\dev\FSharp\AdventOfCode\Day19\input.txt"

    let initialMemory = readInts inputPath
    //The program uses two input instructions to request the X and Y position to which the drone should be deployed.
    let runPgm (x:int64) (y:int64) =
        let mutable (memory, status) = extendMemory initialMemory MyQueue.empty
        status.phase  <- MyQueue.enqueue status.phase x
        status.phase  <- MyQueue.enqueue status.phase y
        status.suspended <- false
        while not (status.finished) do
            status <- runCmd status &memory
        let output = MyQueue.dequeue &status.output
        (int)output

    // Then, the program will output whether the drone is stationary (0) or being pulled by something (1). 
    // For example, the coordinate X=0, Y=0 is directly in front of the tractor beam emitter, so the drone control program will always report 1 at that location.
    let drone (point:Position) : bool =
        let output = runPgm (int64 point.X) (int64 point.Y)  // stationary (0) or being pulled by something (1)
        output = 1 

    let scanArea =
        [|0..49|] // For each of X and Y, this will be 0 through 49.
        |> Array.map(fun x -> 
            [|0..49|] // For each of X and Y, this will be 0 through 49.
            |> Array.map(fun y ->
                let point = {X=x; Y=y}
                (point, drone point)
            )
        ) 
        |> Array.collect id

    //How many points are affected by the tractor beam in the 50x50 area closest to the emitter?
    let affected = 
        scanArea
        |> Array.filter(fun (_,affected) -> affected)

    affected
    |> Array.length
    |> printfn "Answer1 affected %d" 
    
    printfn "executed in %d ms"  sw.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0