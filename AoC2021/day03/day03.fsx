open System
open System.IO

let sr = new StreamReader("input.txt")

let lines =
    [|
    while (not sr.EndOfStream) do
        yield sr.ReadLine() 
    |]


let groupFreq (line: char[]) =
  line
  |> Array.groupBy id
  |> Array.map(fun (v,l) -> (v, l |> Array.length) )
  |> Map.ofArray

let most_frequent (map: Map<char,int>) =
  match map.ContainsKey('0'), map.ContainsKey('1') with
  | false, true -> '1' 
  | true, false ->  '0'
  | true, true -> if (map.['1'] >= map.['0']) then '1' else '0'

let least_frequent (map: Map<char,int>) =
  match map.ContainsKey('0'), map.ContainsKey('1') with
  | false, true -> '1' 
  | true, false ->  '0'
  | true, true -> if (map.['0'] <= map.['1']) then '0' else '1'

let greek 
  (lines: string[]) (bitmask: string[] -> int -> string -> string[]) (op_freq: Map<char,int> -> char) 
  = 
  [|0..lines.[0].ToCharArray().Length - 1|]
  |> Array.fold(fun mask idx -> 
      bitmask lines idx mask
      |> Array.map (fun l -> l.[idx])
      |> groupFreq
      |> op_freq
      |> fun (c:char) -> 
        mask + string(c)
  ) ""

let simple lines _ _ = lines
  
let mostfreq = greek lines simple most_frequent

let leastfreq = greek lines simple least_frequent

let asBinary(arr: string) =
  Convert.ToInt32(String.Join("",arr.ToCharArray()),2) 

let part1 = asBinary(mostfreq) * asBinary(leastfreq)

printfn "part 1: %i * %i = %i" (asBinary(mostfreq)) (asBinary(leastfreq)) part1


let bitmask (lines: string[]) (idx:int) (mask:string) = 
      if idx = 0 then lines 
      else 
        lines 
        |> Array.filter (fun line ->
          line.Substring(0,idx) = mask 
        )

let mostfreqmasked = greek lines bitmask most_frequent

let leastfreqmasked = greek lines bitmask least_frequent

let part2x = asBinary mostfreqmasked 
let part2y = asBinary leastfreqmasked
let part2 = part2x * part2y
printfn "part 2: %i * %i = %i" part2x part2y part2

