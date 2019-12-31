open System
open System.IO
open Checked
open System.Diagnostics


let parse (str:string) : bool[] =
    str.ToCharArray()
    |> Array.map(fun c -> c = '#')

//https://adventofcode.com/2019/day/24 part 2
[<Literal>]
let Centre = 12

let rec pick (me:int) (i:int) (flat_num:int) (flats:bool[][]) : int =
    let flat = flats.[flat_num]
    match i with
    | Centre -> 
        if flat_num = 0 then 0 else
        match me with
        | 7 -> 
            (pick me 0 (flat_num-1) flats) +
            (pick me 1 (flat_num-1) flats) +
            (pick me 2 (flat_num-1) flats) +
            (pick me 3 (flat_num-1) flats) +
            (pick me 4 (flat_num-1) flats)
        | 11 -> 
            (pick me 0 (flat_num-1) flats) +
            (pick me 5 (flat_num-1) flats) +
            (pick me 10 (flat_num-1) flats) +
            (pick me 15 (flat_num-1) flats) +
            (pick me 20 (flat_num-1) flats)
        | 13 -> 
            (pick me 4 (flat_num-1) flats) +
            (pick me 9 (flat_num-1) flats) +
            (pick me 14 (flat_num-1) flats) +
            (pick me 19 (flat_num-1) flats) +
            (pick me 24 (flat_num-1) flats)
        | 17 -> 
            (pick me 20 (flat_num-1) flats) +
            (pick me 21 (flat_num-1) flats) +
            (pick me 22 (flat_num-1) flats) +
            (pick me 23 (flat_num-1) flats) +
            (pick me 24 (flat_num-1) flats)
        | _ -> failwith <| sprintf "can't pick central tile from %d" me
    | i when i >= flat.Length -> 
        if flat_num = flats.Length - 1 then 0 else
            pick me 17 (flat_num+1) flats
    | i when i < 0 -> 
        if flat_num = flats.Length - 1 then 0 else
            pick me 7 (flat_num+1) flats
    | i -> if flat.[i] then 1 else 0

let adjacent(i:int) (flat_num:int) (flats:bool[][]) : int =
    //let flat = flats.[flat_num]
    (pick i (i-5) flat_num flats) +
    (pick i (i+5) flat_num flats) +
    (if i % 5 = 0 then 
        if flat_num = flats.Length - 1 then 0 else
            pick i 11 (flat_num+1) flats
     else pick i (i-1) flat_num flats) +
    (if i % 5 = 4 then 
        if flat_num = flats.Length - 1 then 0 else
            pick i 13 (flat_num+1) flats
     else pick i (i+1) flat_num flats)

let next (flat_num:int) (flats:bool[][]) : bool[] =
    let flat = flats.[flat_num]
    if flat.[Centre] then failwith "central tile must be empty"
    flat
    |> Array.mapi( fun i -> 
        function
        | _ when i = Centre -> false
        | false -> 
            let adj = adjacent i flat_num flats
            2 >= adj && adj >= 1
        | true -> 
            adjacent i flat_num flats = 1
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

let bugs1L (flat:bool[]) =
    flat
    |> Array.filter id
    |> Array.length

let bugs (flats:bool[][]) =
    flats
    |> Array.sumBy bugs1L

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

    let mutable minutes = 0
    let max_minutes = 200
    let debug = false
    let answer2 =
        Array.unfold
            (fun s -> 
                if minutes = max_minutes
                then 
                    if debug 
                    then
                        s
                        |> Array.rev
                        |> Array.iteri ( fun i tile ->
                            printfn "level %d " (i - s.Length/2)
                            show tile)
                    None 
                else
                minutes <- minutes + 1
                let sandwich = 
                    ( if (s |> Array.last |> Array.exists id) then [|Array.create 25 false|] else [||])
                    |> Array.append s
                    |> Array.append ( if (s |> Array.head |> Array.exists id) then [|(Array.create 25 false)|] else [||])
                let next =
                    sandwich
                    |> Array.mapi(fun i_num _ ->
                        next i_num sandwich
                    )
                Some (bugs next, next))
            [|flat|]
        |> Array.last
        
    sw.Stop()
    printfn "Answer Part 2 is %d\n\nexecuted in %d ms" answer2 sw.ElapsedMilliseconds
    
    Console.ReadKey() |> ignore
    0    