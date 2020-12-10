module Common


open System.IO

let cleanLine (str:string) =
    str.Replace("\n","").Replace("\r","").Trim()

let getLines filename =
    let folder = @"C:\dev\FSharp\AoC\AoC2020\"
    let sr = new StreamReader(Path.Combine(folder,  filename))

    [| 
        while not sr.EndOfStream do
        yield cleanLine <| sr.ReadLine() 
    |]
let getContent filename =
    let folder = @"C:\dev\FSharp\AoC\AoC2020\"
    let sr = new StreamReader(Path.Combine(folder,  filename))
    sr.ReadToEnd()

let cleanLastEmptyLine (txt:string) =
    if  txt.EndsWith("\n")
    then txt.Substring(0, txt.Length - 1)
    else txt

let analyse liner (lines: string[]) = lines |> Array.map liner


let generator  analyse liner step start lines debug = 
    analyse liner lines 
    |> Array.fold step start

let debugger liner step start lines=
    generator  analyse liner step start lines true
let solver  liner step start lines =
    generator  analyse liner step start lines false


type State = {result:int; xpos:int; ypos:int; curr_row:int; curr_list: int list}
let start ={result=0; xpos=0; ypos=0; curr_row=0; curr_list = []}

let phaseState step move liner (lines: string[]) =
    let cols = lines.[0].Length
    let rows = lines.Length
    solver
           liner (step cols rows move) start lines 

let phase step move liner (lines: string[]) =
    (phaseState step move liner lines).curr_list

let phaseResult step move liner (lines: string[]) =
    (phaseState step move liner lines).result