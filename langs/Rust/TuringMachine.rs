use std::collections::HashMap;

fn main() {
    // Define the tape with some initial input
    let mut tape: Vec<char> = vec!['1', '1', '0', '1']; // Initial input
    let mut head: usize = 0;                           // Initial head position
    let mut state = "q0".to_string();                  // Initial state

    // Define the transition table
    let mut transitions: HashMap<(&str, char), (&str, char, isize)> = HashMap::new();
    transitions.insert(("q0", '1'), ("q1", '1', 1));
    transitions.insert(("q0", '0'), ("q2", '1', 1));
    transitions.insert(("q1", '1'), ("q0", '1', 1));
    transitions.insert(("q1", '0'), ("q1", '1', 1));
    transitions.insert(("q2", '1'), ("q2", '1', 1));
    transitions.insert(("q2", '0'), ("HALT", '0', 0));

    // Function to run the Turing machine
    while state != "HALT" {
        // Get the current symbol on the tape
        let symbol = if head < tape.len() { tape[head] } else { '0' };

        // Determine the action based on the current state and symbol
        let action = transitions.get(&(state.as_str(), symbol));

        // If no action is defined, halt the machine
        if action.is_none() {
            println!("No valid transition found. Halting.");
            break;
        }

        let (new_state, new_symbol, direction) = action.unwrap();

        // Apply the action
        state = new_state.to_string();
        if head < tape.len() {
            tape[head] = *new_symbol;
        } else {
            tape.push(*new_symbol);
        }
        head = (head as isize + direction) as usize;

        // Output the current state of the tape and head position
        println!("State: {}, Tape: {}, Head: {}", state, tape.iter().collect::<String>(), head);
    }

    // Output the final tape
    println!("Final Tape: {}", tape.iter().collect::<String>());
}
