open System
open System.IO

let sr = new StreamReader("input.txt")

let lines =
    [|
    while (not sr.EndOfStream) do
        yield sr.ReadLine() 
    |]

type Move =
| Forward of int
| Up of int
| Down of int

let moves = 
  lines
  |> Array.map(fun l -> 
    let split = l.Split(" ") 
    let cmd = split.[0]
    let num = int (split.[1]) 
    match cmd with
    | "forward" -> Forward num
    | "up" -> Up num
    | "down" -> Down num
  )

let hor, ver = 
  moves
  |> Array.fold 
    (fun (hor, ver) line  ->
      match line with
      | Forward f -> (hor + f, ver)
      | Up u -> (hor, ver - u)
      | Down d -> (hor, ver + d)
    )
    (0,0)

printfn "part 1 %d" (hor *  ver)

let aim, hor2, ver2 = 
  moves
  |> Array.fold 
    (fun (aim, hor, ver) line  ->
      match line with
      | Forward f -> (aim, hor + f, ver + aim * f)
      | Up u -> (aim - u, hor, ver)
      | Down d -> (aim + d, hor, ver)
    )
    (0,0,0)

printfn "part 2 %d" (hor2 *  ver2)

