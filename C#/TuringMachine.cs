using System;
using System.Collections.Generic;

class Transition
{
    public int CurrentState { get; set; }
    public char ReadSymbol { get; set; }
    public char WriteSymbol { get; set; }
    public char Direction { get; set; } // 'L' for left, 'R' for right
    public int NextState { get; set; }

    public Transition(int currentState, char readSymbol, char writeSymbol, char direction, int nextState)
    {
        CurrentState = currentState;
        ReadSymbol = readSymbol;
        WriteSymbol = writeSymbol;
        Direction = direction;
        NextState = nextState;
    }
}

class TuringMachine
{
    private const int TAPE_SIZE = 100;
    private const int HALT_STATE = -1;

    private List<Transition> transitions;
    private char[] tape;
    private int headPosition;
    private int currentState;

    public TuringMachine(string input, List<Transition> transitions)
    {
        tape = new char[TAPE_SIZE];
        Array.Copy(input.ToCharArray(), tape, input.Length);
        this.transitions = transitions;
        currentState = 0;
        headPosition = 0;
    }

    private int FindTransition(int currentState, char readSymbol)
    {
        for (int i = 0; i < transitions.Count; i++)
        {
            if (transitions[i].CurrentState == currentState && transitions[i].ReadSymbol == readSymbol)
            {
                return i;
            }
        }
        return -1;
    }

    public void Run()
    {
        while (currentState != HALT_STATE)
        {
            char currentSymbol = tape[headPosition];
            int transitionIndex = FindTransition(currentState, currentSymbol);

            if (transitionIndex == -1)
            {
                Console.WriteLine("No valid transition found. Halting...");
                break;
            }

            Transition transition = transitions[transitionIndex];

            // Perform the transition
            tape[headPosition] = transition.WriteSymbol;
            headPosition += (transition.Direction == 'R') ? 1 : -1;
            currentState = transition.NextState;

            // Prevent the head from moving out of bounds
            if (headPosition < 0 || headPosition >= TAPE_SIZE)
            {
                Console.WriteLine("Tape head moved out of bounds. Halting...");
                break;
            }
        }
    }

    public string GetTape()
    {
        return new string(tape).TrimEnd('\0');
    }
}

class Program
{
    static void Main(string[] args)
    {
        string input = "0101010101"; // Example input

        // Define the transitions for flipping the bits
        List<Transition> transitions = new List<Transition>
        {
            new Transition(0, '0', '1', 'R', 0),
            new Transition(0, '1', '0', 'R', 0),
            new Transition(0, '_', '_', 'R', HALT_STATE) // Halt on blank symbol (assumes '_' as blank)
        };

        TuringMachine machine = new TuringMachine(input, transitions);

        Console.WriteLine("Initial tape: " + input);
        machine.Run();
        Console.WriteLine("Final tape: " + machine.GetTape());
    }
}
