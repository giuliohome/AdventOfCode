open System
open System.IO
open Checked
open System.Diagnostics
open AoC2019.IntCode


let parse (str:string) : bool[] =
    str.ToCharArray()
    |> Array.map(fun c -> c = '#')

//https://adventofcode.com/2019/day/24

let pick (i:int) (flat:bool[]) : int =
    match i with
    | i when i >= flat.Length || i < 0 -> 0
    | i -> if flat.[i] then 1 else 0

let adjacent(i:int) (flat:bool[]) : int =
    (pick (i-5) flat) +
    (pick (i+5) flat) +
    (if i % 5 = 0 then 0 else pick (i-1) flat) +
    (if i % 5 = 4 then 0 else pick (i+1) flat)

let next (flat:bool[]) : bool[] =
    flat
    |> Array.mapi( fun i -> 
        function
        | false -> 
            let adj = adjacent i flat
            2 >= adj && adj >= 1
        | true -> 
            adjacent i flat = 1
    )
    
let show (flat:bool[]) =
    flat
    |> Array.chunkBySize 5
    |> Array.iter(fun line ->
        line |> Array.iter (fun tile ->
            printf "%s" (if tile then "#" else "."))
        printfn ""
        )

let rating(flat:bool[]) =
    flat
    |> Array.mapi(fun i t->
        if t then (int)(Math.Pow(2.,(float)i)) else 0)
    |> Array.sum

[<EntryPoint>]
let main _ =

    let sw = Stopwatch()
    sw.Start()

    use sr = new StreamReader("C:\dev\FSharp\AoC2019\Day24\input.txt")
    let input =
        [|
            while (not sr.EndOfStream) do
                yield sr.ReadLine() |> parse
        |]
    if input.Length <> 5 then failwith "rows must be 5"
    if input |> Array.exists(fun r -> r.Length <> 5) then failwith "columns must 5"

    let flat = input |> Array.collect id

    let dupl =
        Array.unfold
            (fun s -> 
                let last = s |> Array.last
                if (s 
                    |> Array.filter(fun previous -> 
                        previous = last) 
                    |> Array.length > 1) 
                then None else 
                let nxt = next last
                Some (nxt, Array.append s [|nxt|]))
            [|flat|]
        |> Array.last

    show dupl
    let found = Some (rating dupl)
    sw.Stop()
    match found with
    | Some answer2 -> 
        printfn "Answer Part 2 is %d\n\nexecuted in %d ms" answer2 sw.ElapsedMilliseconds
    | None -> 
        printfn "ops... answer part 2 not found after %d ms" sw.ElapsedMilliseconds
    
    Console.ReadKey() |> ignore
    0    