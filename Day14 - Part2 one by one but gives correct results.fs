// F# solution for
// https://adventofcode.com/2019/day/14

open System
open System.IO
//open Checked
open System.Diagnostics
open System.Collections.Generic

let mutable ar = Dictionary<string,int>()

type Component = {name: string; needs: int} with
    static member Parse (str:string) =
        let parts = str.Trim().Split(' ')
        {name = parts.[1]; needs = int parts.[0];}

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
    member this.perNeed (need:int) : Formula =
        let mult = (int) (Math.Ceiling ((decimal) need / (decimal)this.toComp.needs))
        let name = this.toComp.name
        let sum = if ar.ContainsKey name then ar.[name] else 0
        ar.[name] <- sum + (this.toComp.needs * mult) - need
        {this with
            toComp =
                { this.toComp with 
                    needs = this.toComp.needs * mult}
            fromComps = 
                this.fromComps 
                |> Array.map(fun c -> 
                    let sum1 = if ar.ContainsKey c.name then ar.[c.name] else 0
                    if c.needs * mult > sum1  then
                        ar.[c.name] <- 0
                        {c with needs = c.needs * mult - sum1}
                    else
                        ar.[c.name] <- ar.[c.name] - (c.needs * mult)
                        {c with needs = 0}
                )}
    member this.toOreFrom (c:Component) =
        if this.fromComps.Length <> 1 then failwith "not unique ore"
        if this.fromComps.[0].name <> "ORE" then failwith "must be ore"
        if this.toComp.name <> c.name then failwith "wrong component"
        let need = (int) (Math.Ceiling ( (decimal)c.needs / (decimal)this.toComp.needs))
        ar.[c.name] <- ar.[c.name] + ( need * this.toComp.needs) - c.needs
        need * ((int)this.fromComps.[0].needs)


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
        let sum0 = if ar.ContainsKey(tree.Parent.name) then ar.[tree.Parent.name] else 0
        if sum0 >= tree.Parent.needs then
            ar.[tree.Parent.name] <- sum0 - tree.Parent.needs
            {tree with Parent = {tree.Parent with needs = 0}}
        else 
            ar.[tree.Parent.name] <- 0
            {tree with Parent = {tree.Parent with needs = tree.Parent.needs - sum0}}
    | forms -> 
        if forms.Length <> 1 then failwith "formula is not unique"
        let sum0 = if ar.ContainsKey(tree.Parent.name) then ar.[tree.Parent.name] else 0
        let fixneeds =
            if sum0 >= tree.Parent.needs then
                ar.[tree.Parent.name] <- sum0 - tree.Parent.needs
                0
            else 
                ar.[tree.Parent.name] <- 0
                tree.Parent.needs - sum0
        let parent = forms.[0].perNeed fixneeds
        {
            tree with 
                Children = 
                parent.fromComps 
                |> Array.map(fun f -> 
                    //printfn "child %s" f.name
                    let sum1 = if ar.ContainsKey(f.name) then ar.[f.name] else 0
                    if sum1 >= f.needs then
                        ar.[f.name] <- sum1 - f.needs
                        treeFrom formulas {Parent = {f with needs = 0}; Children = [||]}
                    else
                        ar.[f.name] <- 0
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
    let mutable needed = (int64) answer1
    let mutable countFuel = 1
    let mutable before = ar.Keys |> Seq.map(fun k -> (k, ar.[k])) |> Seq.toArray
    let mutable before_needed = 0L

    while (needed <= tri) do
        before <- ar.Keys |> Seq.map(fun k -> (k, ar.[k])) |> Seq.toArray
        let fuelTree = 
            {Parent = fuelFormula.toComp; Children = [||]}
            |> treeFrom formulas

        let comps = toCompsFor fuelTree

        let grouped =
            groupComps comps
        before_needed <- tri - needed
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
                    (int64)(formulaOre.toOreFrom c)
            ))
        
        countFuel <- countFuel + 1
        if (countFuel % 100000 = 0) then printfn "producing %d fuel" countFuel


    printfn "The answer for Part 2 is %s (with ore %s)" ((countFuel-1).ToString()) (before_needed.ToString())
    

    before
    |> Seq.iter(fun (a, b) -> printfn "%s %d" a b)


    timer.Stop()

    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0 