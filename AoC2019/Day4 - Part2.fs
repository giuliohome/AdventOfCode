open System
open System.IO
open System.Diagnostics

[<EntryPoint>]
let main _ =
    let timer = new Stopwatch()
    timer.Start()

    let input = "372304-847060"
    let fromStr = input.Split('-').[0]
    let toStr = input.Split('-').[1]
    let fromNum = Int32.Parse fromStr
    let toNum = Int32.Parse toStr

    let count = 
        [fromNum..toNum]
        |> List.filter(fun x ->
            let test = string x
            let len = test.Length
            let initial = test.[0]
            (test.Substring(1).ToCharArray())
            |> Array.fold
                (fun (ok, (prev, old_ok1, old_ok2, i)) curr ->
                    //Two adjacent digits are the same
                    //the two adjacent matching digits are not part of a larger group of matching digits
                    let break_before =
                        (i = 1) || ((int test.[i-1]) <> (int test.[i-2]) )
                    let break_after =
                        (i = len - 1) || ((int test.[i]) <> (int test.[i+1])) 
                    let ok1 = 
                        old_ok1 || 
                        (prev = curr && break_before && break_after)
                    //Going from left to right, the digits never decrease
                    let ok2 = old_ok2 && ((int curr) >= (int prev)) 
                    (ok1 && ok2, (curr, ok1, ok2, i + 1))
                )
                (false, (initial, false, true, 1))
            |> fst
        )
        |> List.length

    printfn "Answer is: %d (on %d)" count (toNum - fromNum + 1)
    //Answer is: 475 (on 474757)
    //done in (948) ms ! in debug
    // 475 is right
    //Answer is: 297 (on 474757)
    //done in (435) ms !


    timer.Stop()
    printfn "done in (%d) ms !" timer.ElapsedMilliseconds
    Console.ReadKey() |> ignore
    0    
