open System
open System.IO

let sr = new StreamReader("input.txt")
let lines =
    [|
    while (not sr.EndOfStream) do
        yield sr.ReadLine() 
    |]

let toInts (str:string) : int[] =
  str.ToCharArray()
  |> Array.map (int << string)

let inputs: int[][] = 
  lines 
  |> Array.map toInts

let vlen, hlen = inputs |> Array.length, inputs.[0] |> Array.length

let adj1 (l:int) (i:int) : int[] =
  match i with
  | 0 -> [|1|]
  | i when i = l-1 -> [|l-2|]
  | _ -> [|i-1;i+1|]

let adj (i:int) (j:int) : (int * int)[] =
  Array.append
    [| for ii in adj1 vlen i do
         yield (ii,j) |] 
    [| for jj in adj1 hlen j do
         yield (i,jj) |]

[| for i in [|0..vlen-1|] do
     for j in [|0..hlen-1|] do
       yield (i,j) |]
|> Array.fold (fun min_s (i,j) ->
  if (inputs.[i].[j] < 9)
  then
    if 
      (([| 
        for (ii,jj) in adj i j do
          yield inputs.[ii].[jj]
      |] |> Array.min) > inputs.[i].[j])
      then Array.append min_s [|inputs.[i].[j] + 1|]
      else min_s
  else min_s
  ) [||]
|> Array.sum
|> printfn "part 1: %i" // 423

// loeb based part 2 for day 9 
// https://github.com/mstksg/advent-of-code-2021/commit/fee89a284e7096da3a5336647b8e0ffbef347b9c
// https://github.com/quchen/articles/blob/master/loeb-moeb.md#loeb
// map of points to their associated low points after flowing
// all the way downhill
let rec loeb ((i:int), (j:int)) : int*int =
    adj i j
    |> Array.fold (fun (i, j) (ii,jj) ->
      if (inputs.[ii].[jj] < inputs.[i].[j])
      then (loeb( ii, jj)) else (i,j)
      ) (i,j) 
  
[| for i in [|0..vlen-1|] do
     for j in [|0..hlen-1|] do
       if (inputs.[i].[j] < 9) then yield (i,j) |]
|> Array.map loeb
|> Array.groupBy id 
|> Array.map (snd >> Array.length)
|> Array.sortByDescending id
|> Array.take 3
|> Array.fold (*) 1
|> printfn "part2: %i" // 1198704

