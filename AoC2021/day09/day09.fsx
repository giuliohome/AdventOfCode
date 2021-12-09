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

let rec sizebasin (i:int) (j:int) : int =
  if (inputs.[i].[j] < 9) then
    inputs.[i].[j] <- 9
    [| 
      for (ii,jj) in (adj i j) do
        yield sizebasin ii jj 
    |]
    |> Array.sum 
    |> (+) 1
  else 0
  
[| for i in [|0..vlen-1|] do
     for j in [|0..hlen-1|] do
       yield (i,j) |]
|> Array.fold (fun basins (i,j) ->
  if (inputs.[i].[j] < 9) then
    Array.append basins [| sizebasin i j |]
  else basins
  ) [||]
|> Array.sortBy (fun i -> -i)
|> (fun a -> a.[0] * a.[1] * a.[2])
|> printfn "part2: %i" // 1198704

