package TuringMachine api;

fn determine_action(state: String, symbol: char) -> (String, char, i32) {
    match (state, symbol) {
        case ("q0", '1'): return ("q1", '1', 1);
        case ("q0", '0'): return ("q2", '1', 1);
        case ("q1", '1'): return ("q0", '1', 1);
        case ("q1", '0'): return ("q1", '1', 1);
        case ("q2", '1'): return ("q2", '1', 1);
        case ("q2", '0'): return ("HALT", '0', 0);
        default: return ("HALT", '0', 0);
    }
}

fn main() -> i32 {
    var tape: [char; 10] = ['1', '1', '0', '1'];
    var head: i32 = 0;
    var state: String = "q0";

    while (state != "HALT") {
        var symbol = tape[head];
        var (new_state, write_symbol, direction) = determine_action(state, symbol);
        state = new_state;
        tape[head] = write_symbol;
        head += direction;

        Print("State: {0}, Tape: {1}, Head: {2}", state, tape.join(), head);
    }

    Print("Final Tape: {0}", tape.join());
    return 0;
}
