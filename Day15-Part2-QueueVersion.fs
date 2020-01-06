open System
open System.IO
open Checked
open System.Diagnostics
open System.Collections
open MyQueue
//https://adventofcode.com/2019/day/15   

type Position = {X:int;Y:int}

let swap (x: byref<'a>) (y: byref<'a>) =
    let temp = x
    x <- y
    y <- temp


[<EntryPoint>]
let main _ =

    let sw = Stopwatch()
    sw.Start()

    let a, b = 0, 1
    Array.unfold
        (fun (a, b) ->
        if a >= 3 then None else
        let a = a + 1
        let a,b = b,a 
        let b = b + 2 
        printfn "%d %d" a b
        Some (a + b, (a, b)) ) (a, b)
        |> Array.iter (printfn "a+b=%d")

    printfn "executed in %d ms"  sw.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    