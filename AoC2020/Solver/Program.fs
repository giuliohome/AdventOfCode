open Common
open Day3

let phase1test:State = debugger liner step1 start test 
printfn "test answer is %d" phase1test.result

let phase1run:State = solver liner step1 start lines 
printfn "answer is %d" phase1run.result 

printfn ""  