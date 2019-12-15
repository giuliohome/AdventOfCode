// F# solution for
// https://adventofcode.com/2019/day/14

open System
open System.IO
open Checked
open System.Diagnostics
open System.Collections.Generic

let mutable ar = Dictionary<string,int64>()

type Component = {name: string; needs: int64} with
    static member Parse (str:string) =
        let parts = str.Trim().Split(' ')
        {name = parts.[1]; needs = int64 parts.[0];}

type Formula = {fromComps: Component[]; toComp:Component} with
    static member Parse (str:string) =
        let parts = str.Split("=>")
        {
            toComp = 
                parts.[1] 
                |> Component.Parse;
            fromComps =
                parts.[0].Split(',')
                |> Array.map Component.Parse
        }
    member this.perNeedWithoutWaste (need:int64) : Formula =
        if this.toComp.needs <> 1L then failwith "original fuel formula is not 1"
        {this with
            toComp =
                { this.toComp with 
                    needs = need}
            fromComps = 
                this.fromComps 
                |> Array.map(fun c -> 
                    {c with needs = c.needs * need}
                )}
    member this.perNeed (need:int64) : Formula =
        let mult = (int64) (Math.Ceiling ((decimal) need / (decimal)this.toComp.needs))
        let name = this.toComp.name
        let sum = if ar.ContainsKey name then ar.[name] else 0L
        ar.[name] <- sum + (this.toComp.needs * mult) - need
        {this with
            toComp =
                { this.toComp with 
                    needs = this.toComp.needs * mult}
            fromComps = 
                this.fromComps 
                |> Array.map(fun c -> 
                    let sum1 = if ar.ContainsKey c.name then ar.[c.name] else 0L
                    if c.needs * mult > sum1  then
                        ar.[c.name] <- 0L
                        {c with needs = c.needs * mult - sum1}
                    else
                        ar.[c.name] <- ar.[c.name] - (c.needs * mult)
                        {c with needs = 0L}
                )}
    member this.toOreFrom (c:Component) =
        if this.fromComps.Length <> 1 then failwith "not unique ore"
        if this.fromComps.[0].name <> "ORE" then failwith "must be ore"
        if this.toComp.name <> c.name then failwith "wrong component"
        let need = (int64) (Math.Ceiling ( (decimal)c.needs / (decimal)this.toComp.needs))
        ar.[c.name] <- ar.[c.name] + ( need * this.toComp.needs) - c.needs
        need * this.fromComps.[0].needs


let readInput (path:string) : Formula[] =
    use sw = new StreamReader(path)
    [|
        while (not sw.EndOfStream) do
            yield
                sw.ReadLine()
                |> Formula.Parse
    |]

type FormulaTree = {Parent: Component; Children: FormulaTree[];}
    
let rec treeFrom (formulas: Formula[]) (tree:FormulaTree) : FormulaTree =
    if tree.Children.Length > 0 then failwith "tree already built" else
    //printfn "building tree for %s " tree.Parent.name
    match (formulas |> Array.filter(fun f -> f.toComp.name = tree.Parent.name)) with
    | [||] ->
        let sum0 = if ar.ContainsKey(tree.Parent.name) then ar.[tree.Parent.name] else 0L
        if sum0 >= tree.Parent.needs then
            ar.[tree.Parent.name] <- sum0 - tree.Parent.needs
            {tree with Parent = {tree.Parent with needs = 0L}}
        else 
            ar.[tree.Parent.name] <- 0L
            {tree with Parent = {tree.Parent with needs = tree.Parent.needs - sum0}}
    | forms -> 
        if forms.Length <> 1 then failwith "formula is not unique"
        let sum0 = if ar.ContainsKey(tree.Parent.name) then ar.[tree.Parent.name] else 0L
        let fixneeds =
            if sum0 >= tree.Parent.needs then
                ar.[tree.Parent.name] <- sum0 - tree.Parent.needs
                0L
            else 
                ar.[tree.Parent.name] <- 0L
                tree.Parent.needs - sum0
        let parent = forms.[0].perNeed fixneeds
        {
            tree with 
                Children = 
                parent.fromComps 
                |> Array.map(fun f -> 
                    //printfn "child %s" f.name
                    let sum1 = if ar.ContainsKey(f.name) then ar.[f.name] else 0L
                    if sum1 >= f.needs then
                        ar.[f.name] <- sum1 - f.needs
                        treeFrom formulas {Parent = {f with needs = 0L}; Children = [||]}
                    else
                        ar.[f.name] <- 0L
                        treeFrom formulas {Parent = {f with needs = f.needs - sum1}; Children = [||]}
                ) 
        }


let groupComps (comps:Component[])   = 
    let groups =
        comps
        |> Array.groupBy(fun c -> c.name)
    let grouped =
        groups
        |> Array.map(fun (name,cs) -> 
            {
                name = name 
                
                needs = 
                    cs
                    |> Array.sumBy( fun cs1 -> cs1.needs)
            } 
        )
    grouped


let rec toCompsFor (tree:FormulaTree) : Component[] =
    
    if tree.Children.Length = 0 then [| tree.Parent |] else
    tree.Children
    |> Array.fold
            (fun comps c ->
                Array.append 
                        comps 
                        (toCompsFor c))
            [||]


[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()

    let Allformulas = readInput "C:\dev\FSharp\AoC2019\Day14\input.txt"

    let formulas =
        Allformulas
        |> Array.filter(fun fr ->
            fr.fromComps.[0].name <> "ORE"
        )

    let fuelFormula =
        formulas |> Array.find(fun f -> f.toComp.name = "FUEL")
    let fuelTree = 
        {Parent = fuelFormula.toComp; Children = [||]}
        |> treeFrom formulas

    let comps = toCompsFor fuelTree

    let grouped =
        groupComps comps

    let answer1 =
        grouped
        |> Array.sumBy(
            fun c ->
                let formulaOre =
                    Allformulas 
                    |> Array.find(
                        fun o -> 
                            o.toComp.name = c.name
                            && o.fromComps.[0].name = "ORE"
                    )
                formulaOre.toOreFrom c
        )

    printfn "The answer for Part 1 is %d" answer1
    // Your puzzle answer was 485720
    
    let tri = 1000000000000L
    let mutable needed = 0L
    let mutable countFuel = 0L
    ar <- Dictionary()
    let mutable before = Dictionary(ar)
    let mutable before_needed = 0L
    let mutable target = tri / answer1

    while ((needed <= tri) && (target > 0L)) do
        before <- Dictionary(ar)
        before_needed <- needed
        let fuelTargetFormula = fuelFormula.perNeed target
        let targetformulas =
            formulas |> Array.except [|fuelFormula|] |> Array.append [| fuelTargetFormula|]
        let fuelTree = 
            {Parent = fuelTargetFormula.toComp; Children = [||]}
            |> treeFrom targetformulas

        let comps = toCompsFor fuelTree

        let grouped =
            groupComps comps
        needed <- needed +
            (grouped
            |> Array.sumBy(
                fun c ->
                    let formulaOre =
                        Allformulas 
                        |> Array.find(
                            fun o -> 
                                o.toComp.name = c.name
                                && o.fromComps.[0].name = "ORE"
                        )
                    formulaOre.toOreFrom c
            ))
        
        if (needed <= tri)
        then    
            countFuel <- countFuel + target
            //if countFuel > 3848998L then failwith "error too much fuel!"
        else 
            ar <- Dictionary(before)
            needed <- before_needed
            target <- target / 2L

       
    printfn "The answer for Part 2 is %d" countFuel
    //Your puzzle answer was 3848998


    timer.Stop()

    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0 
