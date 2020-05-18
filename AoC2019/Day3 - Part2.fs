open System
open System.IO
open System.Diagnostics

// first 3247
//Found min cross at steps 48054 X 4537 and Y 4955
//done in (101) ms !

type Point = {X:int; Y:int; Steps: int} with
    override this.ToString() = sprintf "(%d,%d,%d)" this.X this.Y this.Steps
    member this.distance = (abs this.X) + (abs this.Y)

type Step = {Dir:Char; Count: int; } with
    static member Parse (str:string) : Step =
        {Dir = str.[0]; Count = Int32.Parse <| str.Substring(1) }
    member this.toPoint (start:Point) : Point =
        //printfn "step start %s and %c for %d" (string start) this.Dir this.Count
        match this.Dir with
        | 'R' -> 
            {X = start.X + this.Count; Y = start.Y; Steps = start.Steps + this.Count}
        | 'U' -> 
            {X = start.X; Y = start.Y + this.Count; Steps = start.Steps + this.Count}
        | 'L' -> 
            {X = start.X - this.Count; Y = start.Y; Steps = start.Steps + this.Count}
        | 'D' -> 
            {X = start.X; Y = start.Y - this.Count; Steps = start.Steps + this.Count}
        | _ -> failwith "wrong dir"

type Segment = {Start:Point; Step: Step} with
    override this.ToString() = sprintf "(%s, %c %d)" (string this.Start) this.Step.Dir this.Step.Count
    member this.Intersect (other:Segment) : Point Option =
        //printfn "step start %s and %c for %d" (string start) this.Dir this.Count
        match this.Step.Dir with
        | 'R' -> 
            match other.Step.Dir with
            | 'U' ->
                if (other.Start.X >= this.Start.X && other.Start.X <= this.Start.X + this.Step.Count) &&
                   (this.Start.Y >= other.Start.Y && this.Start.Y <= other.Start.Y + other.Step.Count)
                then
                    Some {X = other.Start.X; Y = this.Start.X; 
                          Steps = this.Start.Steps + other.Start.Steps +
                                  (this.Start.Y - other.Start.Y) + (other.Start.X - this.Start.X)}  
                else None
            | 'D' -> 
                if (other.Start.X >= this.Start.X && other.Start.X <= this.Start.X + this.Step.Count) &&
                   (this.Start.Y <= other.Start.Y && this.Start.Y >= other.Start.Y - other.Step.Count)
                then
                    Some {X = other.Start.X; Y = this.Start.X; 
                          Steps = this.Start.Steps + other.Start.Steps +
                                  (other.Start.Y - this.Start.Y) + (other.Start.X - this.Start.X)}  
                else None
            | _ -> None

        | 'U' -> 
            match other.Step.Dir with
            | 'R' ->
                if (this.Start.X >= other.Start.X && this.Start.X <= other.Start.X + other.Step.Count) &&
                   (other.Start.Y >= this.Start.Y && other.Start.Y <= this.Start.Y + this.Step.Count)
                then
                    Some {X = this.Start.X; Y = other.Start.X; 
                          Steps = this.Start.Steps + other.Start.Steps +
                                  (other.Start.Y - this.Start.Y) + (this.Start.X - other.Start.X)}  
                else None
            | 'L' -> 
                if (this.Start.X <= other.Start.X && this.Start.X >= other.Start.X - other.Step.Count) &&
                   (other.Start.Y >= this.Start.Y && other.Start.Y <= this.Start.Y + this.Step.Count)
                then
                    Some {X = this.Start.X; Y = other.Start.X; 
                          Steps = this.Start.Steps + other.Start.Steps +
                                  (other.Start.Y - this.Start.Y) + (other.Start.X - this.Start.X)}   
                else None
            | _ -> None
        | 'D' -> 
            match other.Step.Dir with
            | 'R' ->
                if (this.Start.X >= other.Start.X && this.Start.X <= other.Start.X + other.Step.Count) &&
                   (other.Start.Y <= this.Start.Y && other.Start.Y >= this.Start.Y - this.Step.Count)
                then
                    Some {X = this.Start.X; Y = other.Start.X; 
                          Steps = this.Start.Steps + other.Start.Steps +
                                  (this.Start.Y - other.Start.Y) + (this.Start.X - other.Start.X)}  
                else None
            | 'L' -> 
                if (this.Start.X <= other.Start.X && this.Start.X >= other.Start.X - other.Step.Count) &&
                   (other.Start.Y <= this.Start.Y && other.Start.Y >= this.Start.Y - this.Step.Count)
                then
                    Some {X = this.Start.X; Y = other.Start.X; 
                          Steps = this.Start.Steps + other.Start.Steps +
                                  (this.Start.Y - other.Start.Y) + (other.Start.X - this.Start.X)}   
                else None
            | _ -> None
        | 'L' -> 
            match other.Step.Dir with
            | 'U' ->
                if (other.Start.X <= this.Start.X && other.Start.X >= this.Start.X - this.Step.Count) &&
                   (this.Start.Y >= other.Start.Y && this.Start.Y <= other.Start.Y + other.Step.Count)
                then
                    Some {X = other.Start.X; Y = this.Start.X; 
                          Steps = this.Start.Steps + other.Start.Steps +
                                  (this.Start.Y - other.Start.Y) + (this.Start.X - other.Start.X)}  
                else None
            | 'D' -> 
                if (other.Start.X <= this.Start.X && other.Start.X >= this.Start.X - this.Step.Count) &&
                   (this.Start.Y <= other.Start.Y && this.Start.Y >= other.Start.Y - other.Step.Count)
                then
                    Some {X = other.Start.X; Y = this.Start.X; 
                          Steps = this.Start.Steps + other.Start.Steps +
                                  (other.Start.Y - this.Start.Y) + (this.Start.X - other.Start.X)}  
                else None
            | _ -> None
        | _ -> failwith "wrong dir"


type Line = Step[] 

let line2segments (start:Point) (line:Line) : Segment list =
    line
    |> Array.fold
        (fun (old_seglist, old_start) step ->
            let segnew = {Start = old_start; Step = step}
            let new_start = step.toPoint old_start
            //printfn "step %s start %s" (string step) (string old_start)
            (segnew :: old_seglist, new_start)
        )
        ([], start)
    |> fst |>List.rev
    

let readSteps (path:string) : Line [] =
    use sw = new StreamReader (path)
    [|
        while (not sw.EndOfStream) do
            yield sw.ReadLine().Split(",")
            |> Array.map Step.Parse
    |]


    
    

[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()
    let lines = readSteps @"C:\dev\FSharp\AdventOfCode\Day3\input.txt"
    //lines
    //|> Array.iteri(fun i line  ->
    //    printfn "line %d " i
    //    line
    //    |> Array.iter(fun step -> 
    //        printfn "%c %i" step.Dir step.Count
    //    )
    //)

    let segments =
        lines
        |> Array.map(line2segments {X = 0; Y = 0; Steps = 0})

    //printfn "segments"
    //segments
    //|> Array.iteri(fun i line -> 
    //    printfn "Line %d points:" i
    //    line |> List.iter (string >> printf " %s")
    //)
    
    let crossings =
        segments
        |> Array.fold
            (fun (x_old, i) p_curr ->
                let x_new =
                    segments
                    |> Array.mapi (fun i_other p_other ->
                      if i = i_other then [||] else
                      let r =
                          p_curr
                          |> List.choose(fun p1 ->
                            let intersects =
                                p_other 
                                |> List.choose (fun p2 -> p1.Intersect p2)
                                |> List.filter (fun i -> i.X <> 0 || i.Y <> 0)
                            match intersects with
                            | e when e |> Seq.length = 0 -> None
                            | intersects -> 
                                (intersects 
                                |> List.minBy(fun p -> p.Steps) )
                                |> Some)
                      [|r |> List.minBy(fun p -> p.Steps)|]
                      ) |> Array.concat  |> Array.minBy(fun p -> p.Steps)
                ([| Array.append [|x_new|] x_old |> Array.minBy(fun p -> p.Steps) |], i + 1))
            ([||],0)
        |> fst

    let cross =
        crossings
        |> Array.minBy(fun p -> p.Steps)
    printfn "Found min cross at steps %d X %d and Y %d " cross.Steps cross.X cross.Y

    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    
