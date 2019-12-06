open System
open System.IO
open System.Diagnostics

type Orbit = {sun: string; earth:string} with
    static member Parse (line:string) =
        let stars = line.Split(')')
        {sun = stars.[0]; earth = stars.[1]}

let readOrbits (path:string) : Orbit[] =
    use sw = new StreamReader (path)
    [|
        while (not sw.EndOfStream) do
            yield Orbit.Parse(sw.ReadLine())
    |]  


let scenario (input: Orbit[]) : int  =
    let start = (0, [|"COM"|], 0) 
    List.unfold 
        (fun (all: int , old_ext: string[], max: int) ->
            let new_orbits = 
                input
                |> Array.filter(fun o -> old_ext |> Array.contains o.sun)
                |> Array.map(fun o -> o.earth)
            let count = new_orbits.Length * (max + 1)
            if count = 0 then None
            else Some (all + count, (all + count, new_orbits, max + 1))
        ) start
    |> List.last
    

[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()
    let input = readOrbits @"C:\dev\FSharp\AoC2019\Day6\input.txt"
    let output = scenario input 
    printfn "The answer is: %d" output
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    
