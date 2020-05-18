open System
open System.IO
open Checked
open System.Diagnostics
//https://adventofcode.com/2019/day/19
open MyQueue
open AoC2019.IntCode

type Position = {X:int;Y:int} with
    member this.Next = {this with X = this.X + 1}

//Part 2
type SearchStatus =
    {point:Position; currentLength:int; currRowLengths:int[]}
type Searching =
| GoOn of SearchStatus
| StopHere 
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

    let paint (area: Map<Position, bool>) =
        for y in 0..49  do
            [|0..49|]
            |> Array.map(fun x -> 
                (if area.[{X=x; Y=y}] then "x" else "."))
            |> fun line -> String.Join("",line) + (if line |> Array.contains "x" then line |> Array.findIndex (fun c -> c = "x") |> sprintf "%04i" else "0000")
            |> printf "%04i %s" y
            printfn ""

    let reshapeStatus (len:int) (shiftX:int) (currRowLen:int) (currRowLengths: int[]) =
        Array.append 
            [|currRowLen|] 
            (currRowLengths 
             |> Array.takeWhile (fun prevLen -> prevLen - shiftX >= len)
             |> Array.map (fun prevLen -> prevLen - shiftX))

    let part2 (len:int) : Searching -> (Position * Searching) option 
        = function
        | StopHere -> None
        | GoOn status -> 
            if status.currentLength = len then None else
            let currY = status.point.Y + 1
            let mutable currPoint = {X = status.point.X; Y = currY}
            while (drone currPoint |> not) do
                currPoint <- currPoint.Next
            let currX = currPoint.X  
            let shiftX = currX - status.point.X
            while  (drone currPoint.Next) do
                currPoint <- currPoint.Next
            let currRowLen = currPoint.X - currX + 1
            let reshaped = reshapeStatus len shiftX currRowLen status.currRowLengths
            let currLen, currRowLens =
                if (currRowLen - shiftX >= len && (status.currRowLengths |> Array.forall (fun prevLen -> prevLen - shiftX >= len)))
                then status.currentLength + 1, reshaped
                else 
                    if currRowLen >= len
                    then 
                        reshaped.Length, reshaped
                    else 0, [||]
            let point =  {X = currX; Y = currY}
            Some (point, GoOn { point = point; 
                                currentLength = currLen; 
                                currRowLengths = currRowLens })

    let searchBox (len: int) =
        Seq.unfold
            (part2 len)
            (   GoOn {point = {X = 0; Y = 4}; // by visual inspection, skipping all first possible empty rows
                currentLength = 0; currRowLengths = [||]})

    //How many points are affected by the tractor beam in the 50x50 area closest to the emitter?
    let affected = 
        scanArea
        |> Array.filter(fun (_,affected) -> affected)

    affected
    |> Array.length
    |> printfn "Answer1 affected %d" 

    paint (scanArea |> Map.ofArray)
    
    let len = 100
    let found = 
        searchBox len
        |> Seq.last
    let found = {found with Y = found.Y - len + 1}
    printfn "Box of len %d at point (%d, %d) => Answer2 is %d" len found.X found.Y (found.X * 10000 + found.Y)


    printfn "executed in %d ms"  sw.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0