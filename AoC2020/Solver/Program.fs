open Common
open Day3

let phase1test = phase1 true test 
printfn "test answer 1 is %d" phase1test

let phase1run = phase1 false lines 
printfn "answer 1 is %d" phase1run 

let phase2test:int = phase2 true test 
printfn "test answer 2 is %d" phase2test

let phase2run:int = phase2 false lines 
printfn "answer 2 is %d" phase2run 

printfn ""  