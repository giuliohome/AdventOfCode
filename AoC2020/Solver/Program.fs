open Common
open Day5


let phase1run = phase1 lines 
printfn "answer 1 is %d" phase1run 



let phase2run:int = phase2 lines 
printfn "answer 2 is %d" phase2run 

System.Console.ReadKey() |> ignore