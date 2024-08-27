open System

let mutable tape = [| '1'; '1'; '0'; '1' |]  // Initial input on the tape
let mutable head = 0                         // Initial head position
let mutable state = "q0"                     // Initial state

let determineAction (state: string) (symbol: char) =
    match state, symbol with
    | "q0", '1' -> "q1", '1', 1
    | "q0", '0' -> "q2", '1', 1
    | "q1", '1' -> "q0", '1', 1
    | "q1", '0' -> "q1", '1', 1
    | "q2", '1' -> "q2", '1', 1
    | "q2", '0' -> "HALT", '0', 0
    | _ -> "HALT", '0', 0

// Main loop to run the Turing machine
while state <> "HALT" do
    let symbol = if head < tape.Length then tape.[head] else '0'
    let (newState, writeSymbol, direction) = determineAction state symbol
    state <- newState
    if head < tape.Length then tape.[head] <- writeSymbol else tape <- Array.append tape [| writeSymbol |]
    head <- head + direction

    // Output the current state of the tape and head position
    printfn "State: %s, Tape: %s, Head: %d" state (String.Concat tape) head

// Output the final tape
printfn "Final Tape: %s" (String.Concat tape)
