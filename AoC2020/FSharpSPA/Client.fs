namespace FSharpSPA

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating

[<JavaScript>]
module Client =
    open WebSharper.UI.Html
    open WebSharper.UI.Client
    open WebSharper.JQuery
    open WebSharper.UI.Client

    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type IndexTemplate = Template<"index.html", ClientLoad.FromDocument>

    let People =
        ListModel.FromSeq [
            "John"
            "Paul"
        ]
    
    let AoC = 
        let inputVar = Var.Create<string option> None
        div [] [
            h1 [] [text "Let's start with Advent of Code"]
            p [] [text "Quick test"]
            a [
                attr.href "  https://adventofcode.com/2019/day/1"
                attr.target "_blank"
            ] [text "visit advent of code 2019 day 1"] 
            br [] []
            Doc.Button "Grab input day 1 part 1 2019" [] (fun () ->
                let settings = AjaxSettings()
                settings.BeforeSend <-
                        fun (req : JqXHR) (_: AjaxSettings) -> 
                            req.SetRequestHeader("crossDomain","")
                settings.CrossDomain <- true
                settings.Success <- fun data  _  _ ->
                        let data = As<string> data
                        JQuery.Of("#inputText").Val(data).Ignore
                        inputVar.Set(Some data)

                JQuery.Ajax("Content/test_input.txt",settings )
                    .Done(
                        fun () -> Console.Log("Done")
                    ) 
                    |> ignore
            )
            br [] []
            textarea [attr.id "inputText"; attr.style "height:100px"] []
            br [] []
            Doc.BindView
                (
                    function
                    | None -> Doc.Empty
                    | Some input -> 
                        Doc.Button "Solve it!" [] (
                            fun () ->
                                Console.Log(input)
                        )
                )
                inputVar.View
        ]

    [<SPAEntryPoint>]
    let Main () =
        IndexTemplate.AoC2020()
            .QuickTest(AoC)
            .Doc()
        |> Doc.RunById "aoc"
        let newName = Var.Create ""
        IndexTemplate.Main()
            .ListContainer(
                People.View.DocSeqCached(fun (name: string) ->
                    IndexTemplate.ListItem().Name(name).Doc()
                )
            )
            .Name(newName)
            .Add(fun _ ->
                People.Add(newName.Value)
                newName.Value <- ""
            )

            .Doc()
        |> Doc.RunById "main"
