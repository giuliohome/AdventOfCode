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