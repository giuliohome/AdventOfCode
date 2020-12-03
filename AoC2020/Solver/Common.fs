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

let analyse liner (lines: string[]) = lines |> Array.map liner


let generator  analyse liner step start lines debug = 
    analyse liner lines 
    |> Array.fold (step debug) start

let debugger liner step start lines=
    generator  analyse liner step start lines true
let solver  liner step start lines =
    generator  analyse liner step start lines false