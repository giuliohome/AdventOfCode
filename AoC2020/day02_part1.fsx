open System.IO;;
open System;;
open System.Text.RegularExpressions;;

let sr = new StreamReader("input_day02.txt");;

let lines = [| 
    while not sr.EndOfStream do
        yield sr.ReadLine() |];;
     
let analyse (lines: string[]) = lines |> Array.map (fun l -> l.Split(' '));;

let nums = analyse lines;; 

let okPsw (nums: string[][]) =
    nums 
    |> Array.fold(fun counter x -> 
            let letter = x.[1].Substring(0,1)
            let minTimes = x.[0].Split('-').[0] |> int
            let maxTimes = x.[0].Split('-').[1] |> int
            let psw = x.[2]
            let matches = Regex.Matches(psw, letter).Count;
            if (matches >= minTimes && matches <= maxTimes)
            then counter + 1 else counter
        )
        0

let test = @"1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc".Split('\n') |> analyse;;

printfn "test answer is %d" (okPsw test);;
printfn "answer is %d" (okPsw nums) ;;
      