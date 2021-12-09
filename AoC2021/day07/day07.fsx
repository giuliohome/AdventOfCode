open System
open System.IO

let sr = new StreamReader("input.txt")

let lines =
    [|
    while (not sr.EndOfStream) do
        yield sr.ReadLine() 
    |]

let inputs: int[] = 
  lines.[0].Split(",") 
  |> Array.map int

let costs (weight: int -> int -> int) (inp: int[]) (i:int): int =
  inp
  |> Array.map (weight i)
  |> Array.sum

let mini, maxi = inputs |> Array.min, inputs |> Array.max

let solveBy (weight: int -> int -> int) =
  [|mini..maxi|]
  |> Array.map (costs weight inputs)
  |> Array.min  

let weight1 (i:int) (j:int) = 
  Math.Abs(i-j)
let weight2 (i:int) (j:int) = 
  Math.Abs(i-j)*(Math.Abs(i-j) + 1)/2
  
solveBy weight1
|> printfn "part 1 %i" 
solveBy weight2
|> printfn "part 2 %i" 

