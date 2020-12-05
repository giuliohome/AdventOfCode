namespace FSharpSPA
open System.IO
open WebSharper
[<JavaScript>]
module Year2020Common =

    let cleanLine (str:string) =
        str.Replace("\n","").Replace("\r","").Trim()


    let analyse liner (lines: string[]) = lines |> Array.map liner


    let generator  analyse liner step start lines = 
        analyse liner lines 
        |> Array.fold step start


    let solver  liner step start lines =
        generator  analyse liner step start lines

    type State = {result:int; xpos:int; ypos:int; curr_row:int; curr_list: int list}
    let start ={result=0; xpos=0; ypos=0; curr_row=0; curr_list = []}

    let phase step move liner (lines: string[]) =
        let cols = lines.[0].Length
        let rows = lines.Length
        let s = 
           solver
               liner (step cols rows move) start lines 
        s.curr_list