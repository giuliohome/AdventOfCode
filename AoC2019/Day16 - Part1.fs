// Day 16
//https://adventofcode.com/2019/day/16
open System
open System.IO
open Checked

//Each element in the new list is built 
//by multiplying every value in the input list by a value 
//in a repeating pattern and then adding up the results.
let buildList (input: int list) (pattern: int list) : int list =
    input
    |> List.mapi(
        fun inputIdx inputVal ->
        //When applying the pattern, skip the very first value exactly once. 
        //(In other words, offset the whole pattern left by one.)
        let patternIdx = (inputIdx + 1) % pattern.Length
        //Then, only the ones digit is kept: 38 becomes 8, -17 becomes 7, and so on.
        (inputVal * pattern.[patternIdx]) % 10
        )

//The base pattern is 0, 1, 0, -1. 
let basePattern : int list = [0; 1; 0; -1]

//Then, repeat each value in the pattern a number of times equal to the position in the output list being considered. 
//Repeat once for the first element, twice for the second element, three times for the third element, and so on.
let patternForPos 
    (pos:int) // 0 based pos
    (pattern: int list) // the base pattern
    =
    [
    for i in [0..pattern.Length-1] do
        for repeat in [0..pos] do
            yield pattern.[i]
     ]       

let parse (str:string) : int list =
    str.ToCharArray()
    |> Array.map(fun c -> int <| string c)
    |> Array.toList

let phase (input: int list) =
    [
    for pos in 0..input.Length-1 do
        yield 
            basePattern
            |> patternForPos pos
            |> buildList input
            |> List.sum
            |> fun res -> Math.Abs(res % 10)
     ]   
        

let test0 = parse "12345678"
let test1 = parse "80871224585914546619083218645595"
let test2 = parse "19617804207202209144916044189917"
let test3 = parse "69317163492948606335995924319873"

let FFT (n:int) (input: int list) =
    [|1..n|]
    |> Array.fold
        (fun state iter -> 
            if iter % 10 = 0 then printfn "FFT %d" iter
            let state = phase state
            state
        ) input
        
    

[<EntryPoint>]
let main _ =
    
    printfn "initial test"
    [|1..4|]
    |> Array.fold
        (fun state _ -> 
            let state = phase state
            printfn "%A" state
            state
        )
        test0
    |> ignore
    
    printfn "larger tests"
    [test1; test2; test3]
    |> List.iter(fun i ->
        i
        |> FFT 100
        |> List.take 8
        |> printfn "%A")

    use sr = new StreamReader("C:\dev\FSharp\AdventOfCode\Day16\input.txt")
    let answer1 = 
        sr.ReadLine()
        |> parse 
        |> FFT 100
        |> List.take 8

    if not sr.EndOfStream then printfn "input not finished"

    printfn "Answer Part 1 is %A" answer1
     

    let answer2 = 0

    printfn "Answer Part 2 is %d" answer2
    
    Console.ReadKey() |> ignore
    0    
