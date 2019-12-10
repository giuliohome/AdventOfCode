open System
open System.IO

type Point = Coord of int * int 
    with 
    member this.X = 
        let (Coord (x,y)) = this
        x
    member this.Y = 
        let (Coord (x,y)) = this
        y
    static member (-) ((p1:Point),(p2:Point)) =
        Coord (p1.X - p2.X, p1.Y - p2.Y )
    member this.angle = Math.Atan2((float) -this.Y, (float) this.X) // y from top reversed
    member this.square_radius = (this.X * this.X) + (this.Y * this.Y)


let visibileFrom (p1:Point) (map:Point[]) : Point[] =
    map 
    |> Array.except([|p1|])
    |> Array.filter(
        fun p2 ->
            map
            |> Array.except([|p1; p2|])
            |> Array.exists(
                fun p3 ->
                (p2 - p1).angle = (p3 - p1).angle
                &&
                (p2 - p1).square_radius > (p3 - p1).square_radius 
            ) |> not
    )

[<EntryPoint>]
let main _ =

    use sw = new StreamReader("C:\dev\FSharp\AoC2019\DayN\input.txt")
    let input = sw.ReadToEnd()
    let stars =
        input.Split("\n")
        |> Array.mapi(
            fun y line ->
            line.ToCharArray()
            |> Array.mapi(
                fun x c ->
                    if c = '#'
                    then Some (Coord (x,y))
                    else None
            )
        )
        |> Array.concat
        |> Array.choose id
    
    let best =
        stars
        |> Array.map(
            fun p1 ->
                (stars
                |> visibileFrom p1, p1)
        ) 
        |> Array.sortByDescending (fst >> Array.length)
        |> Array.head
        

    let firstAnswer =
        best
        |> fst
        |> Array.length 
    let secondAnswer =
        let guess =
            best 
            |> fst
            |> Array.sortBy(fun p -> 
                let vect = p - (snd best) in
                (
                    match vect.X >= 0, vect.Y <= 0 with
                    | true, true -> 0
                    | true, false -> 1
                    | false, false -> 2
                    | false, true -> 3
                    , // see https://docs.microsoft.com/en-us/dotnet/api/system.math.atan2
                    match vect.Y <= 0 with
                    | true -> -vect.angle
                    | false -> vect.angle
                )
            )
        (guess.[199].X * 100) + guess.[199].Y 
         

    printfn "First answer is: %d" firstAnswer
    printfn "Second answer is: %d" secondAnswer
    Console.ReadKey() |> ignore
    0    