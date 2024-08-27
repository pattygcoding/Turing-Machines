fn determine_action(state string, symbol byte) (string, byte, int) {
    match state {
        'q0' {
            if symbol == `1` {
                return 'q1', `1`, 1
            } else {
                return 'q2', `1`, 1
            }
        }
        'q1' {
            if symbol == `1` {
                return 'q0', `1`, 1
            } else {
                return 'q1', `1`, 1
            }
        }
        'q2' {
            if symbol == `1` {
                return 'q2', `1`, 1
            } else {
                return 'HALT', `0`, 0
            }
        }
        else {
            return 'HALT', `0`, 0
        }
    }
}

fn main() {
    mut tape := [`1`, `1`, `0`, `1`]
    mut head := 0
    mut state := 'q0'

    for state != 'HALT' {
        symbol := if head < tape.len { tape[head] } else { `0` }
        new_state, write_symbol, direction := determine_action(state, symbol)
        state = new_state
        if head < tape.len {
            tape[head] = write_symbol
        } else {
            tape << write_symbol
        }
        head += direction

        println('State: $state, Tape: ${tape.bytestr()}, Head: $head')
    }

    println('Final Tape: ${tape.bytestr()}')
}
