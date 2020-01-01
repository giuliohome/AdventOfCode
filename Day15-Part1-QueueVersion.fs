open System
open System.IO
open Checked
open System.Diagnostics
open AoC2019.IntCode
open System.Collections
open MyQueue
//https://adventofcode.com/2019/day/15   

type Position = {X:int;Y:int}

[<EntryPoint>]
let main _ =

    let sw = Stopwatch()
    sw.Start()

    let mutable (memory, status) = bootstrap "C:\dev\FSharp\AoC2019\Day15\input.txt" MyQueue.empty
    let debug = false
    let mutable found = false
    let mutable distance = 0
    let mutable possible_ways : queue<int64[]> = MyQueue.empty
    let mutable visited : Map<Position,char> = Map.empty
    let mutable position = {X=0;Y=0}

    let track = function
    //because of the reply of 2, you know you've found the oxygen system
        | 2L -> 'O'
     //a reply of 1 means the movement was successful
        | 1L -> '.'
    //If it replies with 0, you know that location is a wall
        | 0L -> '#'
        | _ -> failwith "can't track it"


    let show (visited:Map<Position,char>) =
        let visited_keys = visited |> Map.toArray |> Array.map fst
        let y = visited_keys |> Array.map(fun p -> p.Y)
        let yMin = y |> Array.min
        let yMax = y |> Array.max
        let x = visited_keys |> Array.map(fun p -> p.X)
        let xMin = x |> Array.min
        let xMax = x |> Array.max
        Console.Clear()
        for y in [|yMin..yMax|] |> Array.rev do
            for x in xMin..xMax do
                 visited
                 |> Map.tryFind {X=x;Y=y}
                 |> Option.fold (fun _ c -> c) ' ' 
                 |> printf "%c"
            printfn ""
            

    //Only four movement commands are understood: north (1), south (2), west (3), and east (4).
    let back = function
        | 1L -> 2L
        | 2L -> 1L
        | 3L -> 4L
        | 4L -> 3L
        | _ -> failwith "wrong direction"
    let moveTo = function
        | 1L -> fun position -> {position with Y = position.Y + 1}
        | 2L -> fun position -> {position with Y = position.Y - 1}
        | 3L -> fun position -> {position with X = position.X - 1}
        | 4L -> fun position -> {position with X = position.X + 1}
        | _ -> failwith "wrong direction" 
    for move in [1L..4L] do
        if found then () else 
        status.phase <- MyQueue.enqueue status.phase move
        status.suspended <- false
        while not (status.suspended || status.finished) do
            status <- runCmd status &memory
        // https://stackoverflow.com/questions/59552476/copyofstruct-not-defined?noredirect=1
        // https://github.com/dotnet/fsharp/issues/8069
        let output, q = MyQueue.dequeue status.output
        status.output <- q
        visited <- visited.Add( (position |> moveTo move) , track output )
        match output with 
        //because of the reply of 2, you know you've found the oxygen system
        | 2L -> 
            found <- true
        //a reply of 1 means the movement was successful
        | 1L -> 
            position <- position |> moveTo move
            distance <- distance + 1
            possible_ways <- enqueue possible_ways [|move|]
            status.phase <- MyQueue.enqueue status.phase <| back move
            status.suspended <- false
            while not (status.suspended || status.finished) do
                status <- runCmd status &memory
            let output, q = MyQueue.dequeue status.output
            status.output <- q
            if output <> 1L then failwith "path changed"
            position <- position |> moveTo (back move)
            distance <- distance - 1
        //If it replies with 0, you know that location is a wall and that the droid didn't move
        | 0L -> 
            ()
        | _ -> failwith "wrong output"
    
    let mutable way = [||]
    while not found && MyQueue.length possible_ways > 0 do
        if debug then show visited
        let (way_, possible_ways_) = dequeue possible_ways
        // https://github.com/dotnet/fsharp/issues/8069
        // https://stackoverflow.com/questions/59552476/copyofstruct-not-defined?noredirect=1&lq=1
        way <- way_
        possible_ways <- possible_ways_
        if position.X <> 0 || position.Y <> 0 || distance <> 0 then failwith "position is not initial"

        for move in way do
            status.phase <- MyQueue.enqueue status.phase move
            status.suspended <- false
            while not <| status.suspended || status.finished do
                status <- runCmd status &memory
            let output, q = MyQueue.dequeue status.output
            status.output <- q
            if output <> 1L then failwith "path changed"
            distance <- distance + 1
            position <- position |> moveTo move
        
        for move in [1L..4L] do
            if found then () else 
            if visited.ContainsKey(position |> moveTo move) then () else
            status.phase <-  MyQueue.enqueue status.phase move
            status.suspended <- false
            while not (status.suspended || status.finished) do
                status <- runCmd status &memory
            let output, q = MyQueue.dequeue status.output
            status.output <- q
            visited <- visited.Add( (position |> moveTo move) , track output )
            match output with 
            //because of the reply of 2, you know you've found the oxygen system
            | 2L -> 
                distance <- distance + 1
                found <- true
            //a reply of 1 means the movement was successful
            | 1L -> 
                position <- position |> moveTo move
                distance <- distance + 1
                possible_ways <- enqueue possible_ways <| Array.append way [|move|]
                status.phase <- MyQueue.enqueue status.phase <| back move
                status.suspended <- false
                while not (status.suspended || status.finished) do
                    status <- runCmd status &memory
                let output, q = MyQueue.dequeue status.output
                status.output <- q
                if output <> 1L then failwith "path changed"
                distance <- distance - 1
                position <- position |> moveTo (back move)
            //If it replies with 0, you know that location is a wall and that the droid didn't move
            | 0L -> 
                ()
            | _ -> failwith "wrong output"

        if not found then 
            for move in way |> Array.rev do
                status.phase <-  MyQueue.enqueue status.phase <| back move
                status.suspended <- false
                while not <| status.suspended || status.finished do
                    status <- runCmd status &memory
                let output, q = MyQueue.dequeue status.output
                status.output <- q
                if output <> 1L then failwith "path changed"
                distance <- distance - 1
                position <- position |> moveTo (back move)
    if debug then show visited
    sw.Stop()

    if found then 
        show visited
        printfn "Answer Part 1 is %d" distance
    else printfn "Answer Part 1 not found!"
   
    printfn "executed in %d ms"  sw.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    