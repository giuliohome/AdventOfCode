open System
open System.IO

let readInts (path:string) =
    [|
    use sw = new StreamReader (path)
    while (not sw.EndOfStream) do
        yield sw.ReadLine() |> Int32.Parse
    |]


let swapf f x y = f y x
let fuel =
    swapf (/) 3
    >> swapf (-) 2

let fullFuel =
    Seq.unfold 
        (fuel >> function
        | x when x <= 0 -> None
        | x -> Some (x, x))
    >> Seq.sum
        
    

[<EntryPoint>]
let main argv =
    let input = readInts @"C:\dev\FSharp\AoC2019\Day1\input_part2.txt"
    let fuels : int [] = 
        Array.map fullFuel input
    
    fuels
    |> Array.sum
    |> printfn "Answer: %d" 

    Console.ReadKey() |> ignore
    0