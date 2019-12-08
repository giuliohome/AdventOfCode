open System
open System.IO
open System.Diagnostics

let readInput (path:string)  = 
    [|
        use sw = new StreamReader (path)
        while (not sw.EndOfStream) do
        let layers = [|
            for i in [0..5] do
            let layer = Array.init 25 (fun _ -> ' ') 
            let len = sw.ReadBlock(layer,0,25)
            if len > 0
            then 
                if len = 25 
                then yield layer
                else 
                    printfn "bad char(s) skipped: "
                    [|0..(len - 1)|]
                    |> Array.iter(fun y -> 
                        printf "*%c*(ascii %d)" layer.[y] (int layer.[y])
                    )
                    printfn ""
        |]
        if layers.Length = 6 
        then yield layers
        else
            if layers.Length > 0 
            then
                printfn "bad image: "
                layers |>
                    Array.iter(fun x -> 
                        x |> Array.iter(fun y ->
                            printf "%c(%d)" y (int y)
                        )
                        printfn ""
                    )
                printfn "skipping the above bad image"
    |]

let compute (state: char [] [] ) (t: char [] [] ) : char [] []  =
          [|
            for x in [0..5] do
                yield 
                    [|
                        for y in [0..24] do
                            yield if state.[x].[y] = '2' then t.[x].[y] else state.[x].[y]
                    |]
          |]  

[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()
    let input = readInput @"C:\dev\FSharp\AoC2019\Day8\input.txt"
    let info =
        input
        |> Array.fold (fun state t ->
            compute state t
            )
            ([|
                for i in [0..5] do
                let layer = Array.init 25 (fun _ -> '2') 
                yield layer
            |])


    printfn "The answer is " 
    printfn ""
    info
    |> Array.iter(fun x -> 
        x 
        |> Array.iter(fun y -> printf "%c" (if y = '1' then 'X' else ' ')) 
        printfn ""
    )
    printfn ""
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    
