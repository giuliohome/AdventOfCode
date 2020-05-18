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

let GetNumof (x: char [] []) (c:Char) =
    x
    |> Array.map(fun y ->
        y
        |> Array.filter(fun z -> z = c)
        |> Array.length
    )
    |> Array.sum

[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()
    let input = readInput @"C:\dev\FSharp\AoC2019\Day8\input.txt"
    let info =
        input
        |> Array.map(fun x ->
            (GetNumof x '0', GetNumof x '1', GetNumof x '2')
        )
        |> Array.minBy(fun (a,b,c) -> a)
        |> fun (a,b,c) -> b*c

    printfn "The answer is %d " info
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    
