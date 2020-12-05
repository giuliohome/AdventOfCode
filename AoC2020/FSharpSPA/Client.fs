namespace FSharpSPA

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.UI.Html
open Year2020Day3


[<JavaScript>]
module Client =
    
    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type IndexTemplate = Template<"index.html", ClientLoad.FromDocument>

    //let People =
    //    ListModel.FromSeq [
    //        "John"
    //        "Paul"
    //    ]
    
    let Solve (MySolver: string -> int) (solved:string) (input:string) = 
        JQuery.Of("#" + solved).Val("").Ignore
        let resp = MySolver input
        let text = string resp
        JQuery.Of("#" + solved).Val(text).Ignore
                
    let produceSolution 
        (inputVar:Var<string option>) inputText
        txtDate txtUrl txtLink  txtGrab  inputFile MySolver = 
        div [] [
            
            p [] [text txtDate]
            a [
                attr.href txtUrl
                attr.target "_blank"
            ] [text txtLink] 
            br [] []
            Doc.Button txtGrab [] (fun () ->
                let settings = AjaxSettings()
                settings.BeforeSend <-
                        fun (req : JqXHR) (_: AjaxSettings) -> 
                            req.SetRequestHeader("crossDomain","")
                settings.CrossDomain <- true
                settings.Success <- fun data  _  _ ->
                        let data = As<string> data
                        JQuery.Of("#" + inputText).Val(data).Ignore
                        inputVar.Set(Some data)

                JQuery.Ajax(inputFile,settings )
                    .Done(
                        fun () -> Console.Log("Done")
                    ) 
                    |> ignore
            )
            br [] []
            textarea [attr.id inputText; attr.style "height:100px"] []
            br [] []
            Doc.BindView
                (
                    function
                    | None -> Doc.Empty
                    | Some inputdata -> 
                            Doc.Concat [
                            Doc.Button "Solve it!" [] (
                                fun () ->
                                    let inputdata =  JQuery.Of("#" + inputText).Val() |> string
                                    //Console.Log("input data",inputdata)
                                    Solve MySolver ("solved" + inputText) inputdata
                            )
                            br [] []
                            label [] [text "Solution"]
                            input [attr.id ("solved" + inputText)] []
                        ]

                )
                inputVar.View
        ]


    let AoC = 
        let inputVar2019_02_01 = Var.Create<string option> None
        let inputVar2020_03_01 = Var.Create<string option> None
        let inputVar2020_03_02 = Var.Create<string option> None
        let inputVar2020_04_01 = Var.Create<string option> None
        let inputVar2020_04_02 = Var.Create<string option> None
        let inputVar2020_05_01 = Var.Create<string option> None
        let inputVar2020_05_02 = Var.Create<string option> None
        div [] [
            h2 [] [text "year 2019"]
            produceSolution inputVar2019_02_01 "inputText2019_02_01"
                "day 2 part 1" 
                "https://adventofcode.com/2019/day/2"
                "visit advent of code 2019 day 2"
                "Grab input day 2 part 1 2019"
                "Content/input_2019_02.txt"  Solver.Solve
            h2 [] [text "year 2020"]
            produceSolution inputVar2020_03_01 "inputText2020_03_01"
                "day 3 part 1" 
                "https://adventofcode.com/2020/day/3"
                "visit advent of code 2020 day 3"
                "Grab input day 3 part 1 2020"
                "Content/input_2020_03.txt"  
                (fun txt -> 
                    txt.Split('\n')
                    |> Array.filter (fun l -> l.Trim().Length > 0)
                    |> Array.map Year2020Common.cleanLine
                    |> Year2020Day3.phase1)
            produceSolution inputVar2020_03_02 "inputText2020_03_02"
                "day 3 part 2" 
                "https://adventofcode.com/2020/day/3"
                "visit advent of code 2020 day 3"
                "Grab input day 3 part 2 2020"
                "Content/input_2020_03.txt"  
                (fun txt -> 
                    txt.Split('\n')
                    |> Array.filter (fun l -> l.Trim().Length > 0)
                    |> Array.map Year2020Common.cleanLine
                    |> Year2020Day3.phase2)
            produceSolution inputVar2020_04_01 "inputText2020_04_01"
                "day 4 part 1" 
                "https://adventofcode.com/2020/day/4"
                "visit advent of code 2020 day 4"
                "Grab input day 4 part 1 2020"
                "Content/input_2020_04.txt"  
                (fun txt -> 
                    txt.Split([|"\n\n"|], System.StringSplitOptions.None)
                    |> Array.map(fun s -> s.Replace("\n"," "))
                    |> Array.map Year2020Common.cleanLine
                    |> Year2020Day4.phase1)
            produceSolution inputVar2020_04_02 "inputText2020_04_02"
                "day 4 part 2" 
                "https://adventofcode.com/2020/day/4"
                "visit advent of code 2020 day 4"
                "Grab input day 4 part 2 2020"
                "Content/input_2020_04.txt"  
                (fun txt -> 
                    txt.Split([|"\n\n"|], System.StringSplitOptions.None)
                    |> Array.map(fun s -> s.Replace("\n"," "))
                    |> Array.map Year2020Common.cleanLine
                    |> Year2020Day4.phase2)
            produceSolution inputVar2020_05_01 "inputText2020_05_01"
                "day 5 part 1" 
                "https://adventofcode.com/2020/day/5"
                "visit advent of code 2020 day 5"
                "Grab input day 5 part 1 2020"
                "Content/input_2020_05.txt"  
                (fun txt -> 
                    txt.Split('\n')
                    |> Array.filter (fun l -> l.Trim().Length > 0)
                    |> Array.map Year2020Common.cleanLine
                    |> Year2020Day5.phase1)
            produceSolution inputVar2020_05_02 "inputText2020_05_02"
                "day 5 part 2" 
                "https://adventofcode.com/2020/day/5"
                "visit advent of code 2020 day 5"
                "Grab input day 5 part 2 2020"
                "Content/input_2020_05.txt"  
                (fun txt -> 
                    txt.Split('\n')
                    |> Array.filter (fun l -> l.Trim().Length > 0)
                    |> Array.map Year2020Common.cleanLine
                    |> Year2020Day5.phase2)
        
        ]
    [<SPAEntryPoint>]
    let Main () =
        IndexTemplate.AoC2020()
            .QuickTest(AoC)
            .Doc()
        |> Doc.RunById "aoc"
        //let newName = Var.Create ""
        //IndexTemplate.Main()
        //    .ListContainer(
        //        People.View.DocSeqCached(fun (name: string) ->
        //            IndexTemplate.ListItem().Name(name).Doc()
        //        )
        //    )
        //    .Name(newName)
        //    .Add(fun _ ->
        //        People.Add(newName.Value)
        //        newName.Value <- ""
        //    )

        //    .Doc()
        //|> Doc.RunById "main"
