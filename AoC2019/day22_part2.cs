using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using System.Numerics;

public class AoC
{
	static BigInteger pow(long nnn, long times, long m) {
		return (long)BigInteger.ModPow(new BigInteger(nnn),new BigInteger(times),new BigInteger(m));
		/*long ret = 1;
		nnn = nnn % m;
		while (times > 0) {
		if (times % 2 == 1)
			ret = (ret * nnn) % m;	
		times = times >> 1;
		nnn = (nnn * nnn) % m;
		} 
		return ret;*/
	}


    const Int64 factory =  119315717514047;
    const Int64 times =  101741582076661;
    static int new_stack_count = 0;
    static int wrong_count = 0;
    static int cut_count = 0;
    static int increment_count = 0;
    static BigInteger a = 1;
    static BigInteger b = 0;
    internal static long ReversePlay(string line, long CardPosition) {
	if (line == "deal into new stack")
	{
	    b += 1;
	    a *= -1;
	    b *= -1;
	    a %= factory;
	    b %= factory;
	    new_stack_count++;
	    return factory - 1 - CardPosition;
	}
	if (line.StartsWith("cut "))
	{
	    cut_count++;
	    long n = Int64.Parse(line.Substring(4));
	    //Console.WriteLine("cut {0}", n);
	    b += n;
	    a %= factory;
	    b %= factory;
	    if (n < 0) {
		n = factory + n;
	    }
	    n = n % factory;
	    return (CardPosition + n) % factory;
	}
	if (line.StartsWith("deal with increment "))
	{
	    increment_count++;
	    long n = Int64.Parse(line.Substring("deal with increment ".Length));
	    var p = (pow(n, factory - 2, factory) + factory) % factory;
	    a *= p;
	    b *= p;
	    a %= factory;
	    b %= factory;
	    if (CardPosition % n == 0) return CardPosition / n;
	    long start = 0;
	    long div_acc = 0;
	    long test = -1;
	    for (long smart = 0; smart < 1000000; smart++)
            {
		long div = ((factory - start )/ n) + 1;
		div_acc += div; 
		long rest = start + div *  n - factory;
		long diff = CardPosition - rest;
		if (diff % n == 0) {
			test = (diff / n) + div_acc;
			break;
		}
		start = rest;
	    }
	    //Console.WriteLine("n {0} CardPosition {1} test {2}",n,CardPosition,test);
	    if ( (test * n) % factory == CardPosition  ) return test;
	    long ret = -1;
	    for (long scan = 0; scan < factory; scan++) {
		if (scan % 1000000 == 0) Console.WriteLine("slow scanning {0}", scan);
		if ((scan * n) % factory == CardPosition) {
			ret = n;
			break;
		}
	    }
	    if (ret <0) throw new Exception("incrment not found");
	    return ret;
	}
	Console.WriteLine("wrong line: "+line);
	wrong_count++;
	return CardPosition;
    }
    public static void Main(string[] args)
    {
	Console.WriteLine ("Day 22");
	var content = "";
        using(var sr = new StreamReader("./input_day22.txt"))
        {
	   content = sr.ReadToEnd();
	}
	String[] lines = content.Split('\n').Where(s => s.Trim() != "")
				.Reverse()
				.ToArray();
	Console.WriteLine("Lines: " + lines.Length.ToString());
	long CardPosition = 2020;
	//int num = 0;
	foreach (var line in lines)
	{
	   //Console.WriteLine("line #{0}", num++);
	   CardPosition = ReversePlay(line,CardPosition);
	}
	Console.WriteLine("End of playing cards");
	int check_all = new_stack_count + wrong_count + cut_count + increment_count;
	Console.WriteLine("Stats\n new stack {1}, " + 
		"cut {4}, increment {5}, wrong {2}\n check all {3}",
		-1, new_stack_count, wrong_count, check_all, cut_count, increment_count);
	//Console.WriteLine("a {0} b {1} ", (a + factory)%factory , (b + factory)%factory);
	long a1 = (long)(a + factory)%factory;
	long b1 = (long)(b + factory)%factory;
	Console.WriteLine("a {0} b {1} ", a1 , b1);
	BigInteger answer2 =	(
        pow(a1, times, factory) * 2020 +
        b1 * (pow(a1, times, factory) + factory - 1)
          * (pow(a1 - 1, factory - 2, factory))
	    + factory) % factory;
	Console.WriteLine("answer 2 {0}", answer2);
    }
}
