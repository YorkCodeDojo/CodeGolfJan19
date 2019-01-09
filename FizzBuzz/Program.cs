using System;

namespace FizzBuzz
{
    class Program
    {
        static void Main(string[] args)
        {
            for (int n = 1; n <= 100; n++)
            {
                if (n % 15 == 0)
                    Console.WriteLine("FizzBuzz");
                else if (n % 5 == 0)
                    Console.WriteLine("Buzz");
                else if (n % 3 == 0)
                    Console.WriteLine("Fizz");
                else
                    Console.WriteLine(n);
            }

        }
    }
}
