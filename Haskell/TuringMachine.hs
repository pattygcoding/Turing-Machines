-- Define a type for the tape, which is a list of symbols.
type Tape = [Char]

-- Define a type for the state of the Turing machine.
type State = String

-- Define a type for the head position, which is an integer.
type HeadPosition = Int

-- Define a type for the transition function, which maps a (state, symbol) pair to a new state, a symbol to write, and a direction to move.
type TransitionFunction = ((State, Char) -> (State, Char, Direction))

-- Define a data type for the direction to move the head.
data Direction = L | R deriving (Eq, Show)

-- Define the Turing Machine data type.
data TuringMachine = TuringMachine {
    tape :: Tape,
    headPos :: HeadPosition,
    currentState :: State,
    transitionFunction :: TransitionFunction
}

-- A function to move the head in the specified direction.
moveHead :: HeadPosition -> Direction -> HeadPosition
moveHead pos L = pos - 1
moveHead pos R = pos + 1

-- A function to execute one step of the Turing machine.
step :: TuringMachine -> TuringMachine
step tm@(TuringMachine tape pos state tf) =
    let
        currentSymbol = tape !! pos
        (newState, newSymbol, direction) = tf (state, currentSymbol)
        newTape = take pos tape ++ [newSymbol] ++ drop (pos + 1) tape
        newPos = moveHead pos direction
    in
        TuringMachine newTape newPos newState tf

-- A function to run the Turing machine until it halts.
run :: TuringMachine -> IO ()
run tm@(TuringMachine tape pos state tf)
    | state == "HALT" = putStrLn "Halting." >> print tape
    | otherwise = do
        print tm
        run (step tm)

-- Overriding the `Show` instance to print the TuringMachine in a more readable format.
instance Show TuringMachine where
    show (TuringMachine tape pos state _) =
        "Tape: " ++ show tape ++ "\n" ++
        "Head Position: " ++ show pos ++ "\n" ++
        "Current State: " ++ state ++ "\n"

-- The main function to define the tape, transition function, and run the machine.
main :: IO ()
main = do
    -- Define the tape
    let initialTape = "101__"

    -- Define the transition function
    let tf ("q0", '1') = ("q0", '1', R)
        tf ("q0", '0') = ("q1", '0', R)
        tf ("q0", '_') = ("HALT", '_', L)
        tf ("q1", '1') = ("q1", '0', L)
        tf ("q1", '0') = ("q0", '1', L)
        tf ("q1", '_') = ("HALT", '_', R)
        tf _           = ("HALT", '_', R)

    -- Create the initial Turing machine
    let tm = TuringMachine initialTape 0 "q0" tf

    -- Run the Turing machine
    run tm
