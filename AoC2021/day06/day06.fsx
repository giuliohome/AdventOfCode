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

let initial =
  [|0..8|]
  |> Array.fold(fun m i ->
    m |> Map.add i (
      inputs
      |> Array.filter ((=) i)
      |> Array.length |> int64
      )
  ) Map.empty<int, int64>


let m0 =
  [|0..8|]
  |> Array.fold(fun m i ->
    m |> Map.add i 0L
  ) Map.empty<int, int64>

let step (m:Map<int,int64>) : Map<int,int64> =
  [|0..8|]
  |> Array.fold(fun s i ->
    match i with 
    | 0 -> s |> Map.add 6 (s.[6] + m.[0]) |> Map.add 8 (s.[8] + m.[0])
    | _ -> s |> Map.add (i - 1) (s.[i-1] + m.[i])
  ) m0


let solve n = [|1..n|] |> Array.fold (fun m _ -> step m) initial

let resp n = solve n |> Map.toArray |> Array.sumBy (snd);; 

printfn "part 1 %i" <| resp 80
printfn "part 2 %i" <| resp 256
