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
    let goal = input |> Array.find (fun o -> o.earth = "SAN")
    let you = input |> Array.find (fun o -> o.earth = "YOU") 
    let start = (0, [|you.sun|], [|"YOU"|]) 
    List.unfold 
        (fun (min: int , old_ext: string[], old_visited: string[]) ->
            if old_ext |> Array.contains goal.sun then None else
            let new_orbits_earth = 
                input
                |> Array.filter(fun o -> old_ext |> Array.contains o.sun)
                |> Array.map(fun o -> o.earth)
                |> Array.except(old_visited)
            let new_orbits_sun = 
                input
                |> Array.filter(fun o -> old_ext |> Array.contains o.earth)
                |> Array.map(fun o -> o.sun)
                |> Array.except(old_visited)
            let new_orbits = 
                Array.append new_orbits_earth new_orbits_sun
                |> Array.distinct
            let new_visited = Array.append old_ext old_visited |> Array.distinct 
            Some (min + 1, (min + 1, new_orbits, new_visited))
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
