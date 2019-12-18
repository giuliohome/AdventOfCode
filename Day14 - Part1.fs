// F# solution for
// https://adventofcode.com/2019/day/14

open System
open System.IO
open Checked
open System.Diagnostics
open System.Collections.Generic
open System.Threading.Tasks

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
    
let rec treeFrom (ore_input: Formula[]) (formulas: Formula[]) (tree:FormulaTree) : FormulaTree =
    if tree.Children.Length > 0 then failwith "tree already built" else
    //printfn "building tree for %s " tree.Parent.name
    match (formulas |> Array.filter(fun f -> f.toComp.name = tree.Parent.name)) with
    | [||] ->
        if (ore_input |> Array.exists(fun o -> o.toComp.name = tree.Parent.name) |> not) 
        then failwith "ore missing" 
        tree
    | forms -> 
        if forms.Length <> 1 then failwith "formula is not unique"

        let parent = forms.[0].perNeed tree.Parent.needs
        {
            tree with 
                Children = 
                parent.fromComps 
                |> Array.map(fun f -> 
                    //printfn "child %s" f.name
                    treeFrom ore_input formulas {Parent = f; Children = [||]}
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
    let ore_input =
        Allformulas
        |> Array.filter(fun fr ->
            fr.fromComps.[0].name = "ORE"
        )
    //formulas
    //|> Array.iter (fun f -> printfn "getting %s from %s etc..." f.toComp.name f.fromComps.[0].name )
    //printfn ""

    let fuelFormula =
        formulas |> Array.find(fun f -> f.toComp.name = "FUEL")
    let fuelTree = 
        {Parent = fuelFormula.toComp; Children = [||]}
        |> treeFrom ore_input formulas

    let comps = toCompsFor fuelTree
    
    //comps
    //|> Array.iter (fun c -> printfn "%s %d" c.name c.n)

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
    
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    timer.Restart()
    
    let answer2 = 0
    printfn "The answer for Part 2 is %d" answer2
    
    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0 