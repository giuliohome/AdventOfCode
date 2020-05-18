using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

public class AoC
{
    const int factory = 10007;
    static int new_stack_count = 0;
    static int wrong_count = 0;
    static int cut_count = 0;
    static int increment_count = 0;
    internal static  IEnumerable<int> Play(string line, IEnumerable<int> Cards) {
	if (line == "deal into new stack")
	{
	    new_stack_count++;
	    return Enumerable.Reverse(Cards);
	}
	if (line.StartsWith("cut "))
	{
	    cut_count++;
	    int n = Int32.Parse(line.Substring(4));
	    //Console.WriteLine("cut {0}", n);
	    if (n < 0) {
		n = factory + n;
	    }
	    n = n % factory;
	    var ret = Cards.Skip(n).Concat( Cards.Take(n) ).ToArray();
	    //Console.WriteLine("count after cut {0}", ret.Length);
	    return ret;
	}
	if (line.StartsWith("deal with increment "))
	{
	    increment_count++;
	    int n = Int32.Parse(line.Substring("deal with increment ".Length));
	    int[] ret = new int[factory];
	    int[] input = Cards.ToArray();
	    foreach (int scan in Enumerable.Range(0,factory)) {
		ret[(scan * n) % factory] = input[scan];
	    }
	    return ret;
	}
	Console.WriteLine("wrong line: "+line);
	wrong_count++;
	return Cards;
    }
    public static void Main(string[] args)
    {
	Console.WriteLine ("Day 22");
	var content = "";
        using(var sr = new StreamReader("./input_day22.txt"))
        {
	   content = sr.ReadToEnd();
	}
	String[] lines = content.Split('\n').Where(s => s.Trim() != "").ToArray();
	Console.WriteLine("Lines: " + lines.Length.ToString());
	IEnumerable<int> Cards = Enumerable.Range(0,factory);
	//int num = 0;
	foreach (var line in lines)
	{
	   //Console.WriteLine("line #{0}", num++);
	   Cards = Play(line,Cards);
	}
	Console.WriteLine("End of playing cards");
	if (Cards.Count() !=  factory) Console.WriteLine("Wrong Card Number");
	String test = String.Join(",", 
		Cards.Take(10)
			.Select(i => i.ToString())
			.ToArray());
	int check_all = new_stack_count + wrong_count + cut_count + increment_count;
	Console.WriteLine("first 10: {0}\nStats\n new stack {1}, " + 
		"cut {4}, increment {5}, wrong {2}\n check all {3}",
		test, new_stack_count, wrong_count, check_all, cut_count, increment_count);
	int pos_2019 = Array.FindIndex(Cards.ToArray(),c => c == 2019);
	Console.WriteLine("Answer 1: {0}", pos_2019);
    }
}
