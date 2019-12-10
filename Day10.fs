open System
open System.IO
open System.Diagnostics


let getInput() = [|
        let sr = new StreamReader("C:\dev\FSharp\AdventOfCode\Day10\input.txt")
        while (not sr.EndOfStream) do
            yield
                sr.ReadLine().ToCharArray()
                |> Array.map(fun c -> c='#')
    |]

let sameline 
    (x1:int,y1:int) (x2:int,y2:int) (x3:int,y3:int) 
    : bool = 
            (y1 = y2 && y1 = y3 && x1 <> x2 && x1 <> x3 )
            ||
            ((y1 <> y2) && (y1 <> y3)
                && ((x1 - x2) * (y1 - y3) 
                        = (x1 - x3) * (y1 - y2)))

let points (x1:int,y1:int) (map: (int * int) []) =
    map
    |> Array.except([|(x1,y1)|])
    |> Array.fold(fun p (x2:int,y2:int) ->
            if 
                map 
                |> Array.except([|(x1,y1);(x2,y2)|])
                |> Array.exists(fun (x3:int,y3:int) ->
                        (sameline (x1,y1) (x2,y2) (x3,y3))
                        && ( (x3 > x1 && x3 - x1 < x2 - x1) || ((x3 < x1 && x3 - x1 > x2 - x1) ) || (x1 = x2 && x1 = x3))
                        && ((y3 > y1 && y3 - y1 < y2 - y1) || (y3 < y1 && y3 - y1 > y2 - y1) || (y1 = y2 && y1 = y3))
                    )
            then p 
            else
                //printfn "%A can see %A" (x1,y1) (x2,y2)
                p + 1
        )
        0

let orderedpoints (x1:int,y1:int) (map: (int * int) []) =
    let list = 
        map
        |> Array.except([|(x1,y1)|])
        |> Array.fold(fun (p: (int * int) list) (x2:int,y2:int) ->
                if 
                    map 
                    |> Array.except([|(x1,y1);(x2,y2)|])
                    |> Array.exists(fun (x3:int,y3:int) ->
                            (sameline (x1,y1) (x2,y2) (x3,y3))
                            && ( (x3 > x1 && x3 - x1 < x2 - x1) || ((x3 < x1 && x3 - x1 > x2 - x1) ) || (x1 = x2 && x1 = x3))
                            && ((y3 > y1 && y3 - y1 < y2 - y1) || (y3 < y1 && y3 - y1 > y2 - y1) || (y1 = y2 && y1 = y3))
                        )
                then p 
                else
                    //printfn "%A can see %A" (x1,y1) (x2,y2)
                    (x2,y2) :: p
            )
            []
    list

[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()

    let input = getInput()
    let map =
        [|
            for i in [|0..input.Length - 1|] do
                for j in [|0 .. input.[i].Length - 1|] do
                    if input.[i].[j] then yield (j,i)
        |]
    
    let ((xb,yb), best) = 
        map
        |> Array.fold
            (fun ((x,y),p) (t1,t2) ->
                let t3 = points (t1,t2) map
                //printfn "%A has %d points" (t1,t2) t3
                if (t3 > p) 
                then ((t1,t2),t3)
                else ((x,y),p)
            )
            ((-1,-1),-1)

    printfn "Answer Part 1 is: %d for x %d, y %d"  best xb yb
    //Answer is: 296 for x 17, y 23

    let list = 
        orderedpoints (xb,yb) map
        |> List.sortBy(fun (x,y) -> 
            (
                match x - xb >= 0 , y - yb <= 0 with
                | true, true -> 0
                | true, false -> 1
                | false, false -> 2
                | false, true -> 3
                
            , 
                 match x - xb >= 0 , y - yb <= 0 with
                | true, true -> Math.Atan(((float)(x - xb))/((float)(yb - y)))
                | true, false -> - Math.Atan(((float)(x - xb))/((float)(y - yb)))
                | false, false -> Math.Atan(((float)(xb - x))/((float)(y - yb)))
                | false, true -> - Math.Atan(((float)(xb - x))/((float)(yb - y)))
                
            
            )
        )
    
    //list |> List.iteri(fun i t -> printfn "%d %A" i t)

    printfn "Answer Part 2 is %d for %A" ((fst list.[199])*100 + (snd list.[199])) list.[199]

    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    
