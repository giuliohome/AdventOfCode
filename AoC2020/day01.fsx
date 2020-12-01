open System.IO;;

let sr = StreamReader("input.txt");;

let lines = [ while not sr.EndOfStream do
     yield sr.ReadLine() ];;
     

let nums = lines |> List.map int;;

nums 
|> List.iteri(fun i x -> 
  nums 
  |> List.skip i 
  |> List.iter( fun y -> 
    if x + y = 2020 
    then printfn "%d %d => result is %d" x y (x*y)));;
    
    
nums 
|> List.iteri(fun i x -> 
  nums 
  |> List.skip i 
  |> List.iteri( fun j y -> 
    nums
    |> List.skip (i + j)
    |> List.iter(fun z ->
      if x + y  + z = 2020 
      then printfn "%d %d %d => result is %d" x y z (x*y*z))));;
      