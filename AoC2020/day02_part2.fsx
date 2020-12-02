open System.IO;;
open System;;

let sr = new StreamReader("input_day02.txt");;

let lines = [| 
    while not sr.EndOfStream do
        yield sr.ReadLine() |];;
     
let analyse (lines: string[]) = lines |> Array.map (fun l -> l.Split(' '));;

let nums = analyse lines;; 

let okPsw (nums: string[][]) =
    nums 
    |> Array.fold(fun counter x -> 
            let letter = x.[1].[0]
            let mustHave = (x.[0].Split('-').[0] |> int ) - 1
            let mustNot = (x.[0].Split('-').[1] |> int) - 1
            let psw = x.[2]
            match psw.[mustHave] = letter , (psw.[mustNot] = letter) with
            | true, false -> counter + 1 
            | false, true -> counter + 1
            | _, _ -> counter
        )
        0

let test = @"1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc".Split('\n') |> analyse;;

printfn "test answer is %d" (okPsw test);;
printfn "answer is %d" (okPsw nums) ;;
      