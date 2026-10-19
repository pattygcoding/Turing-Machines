using System;
using System.Text;

class TuringMachine
{
    static void Main()
    {
        string line = Console.ReadLine() ?? "";

        foreach (char character in line)
        {
            if (character != '0' && character != '1')
            {
                Console.WriteLine("Invalid input: expected a binary string.");
                return;
            }
        }

        // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten
        // to 0 and the head moves left (carry); a 0 becomes 1 and the machine
        // halts; a blank becomes 1 (a new leading digit) and the machine halts.
        StringBuilder tape = new StringBuilder(line);
        int head = tape.Length - 1;
        while (true)
        {
            char symbol = head >= 0 ? tape[head] : '_';
            if (symbol == '1')
            {
                tape[head] = '0';
                head -= 1;
            }
            else if (symbol == '0')
            {
                tape[head] = '1';
                break;
            }
            else
            {
                tape.Insert(0, '1');
                break;
            }
        }

        Console.WriteLine("Initial tape: " + line);
        Console.WriteLine("Final tape: " + tape);
    }
}
