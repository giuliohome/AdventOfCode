// Day 16 Part 2
//https://adventofcode.com/2019/day/16
// based on https://github.com/ephemient/aoc2019/blob/py/src/aoc2019/day16.py
open System
open System.IO
open Checked


let parse (str:string) : int[] =
    str.ToCharArray()
    |> Array.map(fun c -> int <| string c)

    

[<EntryPoint>]
let main _ =
    
    use sr = new StreamReader("C:\dev\FSharp\AoC2019\Day16\input.txt")
    let input = 
        sr.ReadLine()
        |> parse 

    let offset = 
        input
            |> Array.take 7
            |> Array.fold
                (fun acc x -> 10 * acc + x)
                0
    let n = 10000 * input.Length - offset
    let valuefolded =
            [|0..( (n - (offset % input.Length) - 1) / input.Length)|]
            |> Array.fold
                (fun acc _ -> Array.append acc input)
                (input
                |> Array.skip (offset % input.Length))

            
    // https://stackoverflow.com/questions/59412516/is-it-possible-to-use-immutable-collections-in-this-case
    [|0..99|]
    |> Array.iter (
        fun repeat ->
            printfn "%d" repeat
            [0..(n-1)] 
            |> List.rev
            |> List.fold (fun acc i ->
                valuefolded.[i] <- Math.Abs(acc + valuefolded.[i]) % 10
                valuefolded.[i]) 0
            |> ignore
        ) 

    let answer2 = valuefolded |> Seq.take 8

    printfn "Answer Part 2 is %s" (String.Join("",answer2))
    
    Console.ReadKey() |> ignore
    0    
