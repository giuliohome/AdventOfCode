open System
open System.IO

type MoonPosition = {x:int; y:int; z: int}

let moons : MoonPosition[] =  [|    
    {x = 9; y = 13; z = -8};
    {x = -3; y = 16; z = -17};
    {x = -4; y = 11; z = -10};
    {x = 0; y = -2; z = -2};
|]
let question1steps = 1000

//let moons : MoonPosition[] =  [|    
//    {x = -1; y = 0; z = 2};
//    {x = 2; y = -10; z = -7};
//    {x = 4; y = -8; z = 8};
//    {x = 3; y = 5; z = -1};
//|]
//let question1steps = 10

type MoonMomentum = 
    {pos: MoonPosition; vx: int; vy:int; vz: int}
    with
    member this.next (others:MoonPosition[]) : MoonMomentum =
        let vx =
            others
            |> Array.sumBy(
                fun other ->
                let diff = (other.x - this.pos.x)
                if diff = 0 then 0 else
                diff / Math.Abs(diff)
            ) 
            |> (+) this.vx
        let vy =
            others
            |> Array.sumBy(
                fun other ->
                let diff = (other.y - this.pos.y)
                if diff = 0 then 0 else
                diff / Math.Abs(diff)
            ) 
            |> (+) this.vy
        let vz =
            others
            |> Array.sumBy(
                fun other ->
                let diff = (other.z - this.pos.z)
                if diff = 0 then 0 else
                diff / Math.Abs(diff)
            ) 
            |> (+) this.vz
        let posx = this.pos.x + vx
        let posy = this.pos.y + vy
        let posz = this.pos.z + vz
        {vx = vx; vy = vy; vz= vz; pos = {x = posx; y = posy; z = posz}}
        
    static member energy (this:MoonMomentum) : int =
        (Math.Abs(this.vx) + Math.Abs(this.vy) + Math.Abs(this.vz)) *
        (Math.Abs(this.pos.x) + Math.Abs(this.pos.y) + Math.Abs(this.pos.z))

type State = 
    {momenta: MoonMomentum[]; steps: int}
    with
    member this.energy() : int =
        this.momenta
        |> Array.sumBy MoonMomentum.energy
    member this.next() : State =
        {
            steps = this.steps + 1
            momenta = 
            this.momenta
            |> Array.map(
                fun momentum ->
                this.momenta
                |> Array.map(
                    fun othermomentum -> 
                    othermomentum.pos)
                |> momentum.next
            )
        }

let start: State  =
    {
        momenta = moons
        |> Array.map(
            fun moon ->
            {pos = moon; vx = 0; vy = 0; vz = 0}
        ); 
        steps = 0
    }



let solver1 (totalsteps: int) 
    : State -> (int * State) option =
    fun state -> 
        if state.steps = totalsteps 
        then None else
        let nextstate = state.next()
        let finalenergy =
            if nextstate.steps = totalsteps 
            then nextstate.energy()
            else 0
        Some (finalenergy, nextstate)
        
let solver2 (start:State)
    : (State * (bool * int) * (bool * int) * (bool * int)) -> 
    ((int * int * int) *(State * (bool * int) * (bool * int) * (bool * int))  ) option =
    fun (state, (foundx, stepsx), (foundy, stepsy), (foundz, stepsz)) -> 
        if (foundx && foundy && foundz)
        then None else
        if state.steps % 100000 = 0 then printfn "arrived at %d steps ..." state.steps
        let foundx = 
            foundx ||
            state.momenta
            |> Array.forall( fun a ->
                start.momenta
                |> Array.exists( fun b ->
                    a.pos.x = b.pos.x
                    &&
                    a.vx = b.vx)
                    )
        let stepsx =
            if stepsx > 0
            then stepsx 
            else
                if foundx
                then state.steps
                else 0

        let foundy = 
            foundy ||
            state.momenta
            |> Array.forall( fun a ->
                start.momenta
                |> Array.exists( fun b ->
                    a.pos.y = b.pos.y
                    &&
                    a.vy = b.vy)
                    )
        let stepsy =
            if stepsy > 0
            then stepsy 
            else
                if foundy
                then
                    state.steps
                else 0

        let foundz = 
            foundz||
            state.momenta
            |> Array.forall( fun a ->
                start.momenta
                |> Array.exists( fun b ->
                    a.pos.z = b.pos.z
                    &&
                    a.vz = b.vz)
                    )
        let stepsz =
            if stepsz > 0
            then stepsz 
            else
                if foundz
                then state.steps
                else 0

        let nextstate = state.next()
        Some ((stepsx, stepsy, stepsz), (nextstate, (foundx, stepsx), (foundy, stepsy), (foundz, stepsz)))


[<EntryPoint>]
let main _ =
    
    let answer1 = 
        Seq.unfold
            (solver1 question1steps)
            start
        |> Seq.last

    printfn "Answer Part 1 is %d" answer1
     
    //part 2 optimization is inspired by the independent axis trick seen in the python by cyphase found below
    //seen in the python by cyphase found on https://www.reddit.com/r/adventofcode/comments/e9j0ve/2019_day_12_solutions/fajp9zu/
    let (stepsx, stepsy, stepsz) = 
        Seq.unfold
            (solver2 start)
            (start.next(), (false, 0), (false, 0), (false, 0))
        |> Seq.last

    let rec gcd (a:int64) (b:int64) =
      if b = (int64)0 
        then abs a
      else gcd b (a % b)

    let lcm (a:int64) (b:int64) : int64 =
        (a * b) / (gcd a b)
         
    let lcm_x_y = lcm ((int64)stepsx) ((int64)stepsy)
    let answer2 = lcm lcm_x_y ((int64)stepsz) 

    printfn "Answer Part 2 is %d" answer2
    
    Console.ReadKey() |> ignore
    0    
