import strutils

var tape: seq[char] = @['1', '1', '0', '1']
var head = 0
var state = "q0"

proc determineAction(state: string, symbol: char): tuple[state: string, writeSymbol: char, direction: int] =
    case (state, symbol)
    of ("q0", '1'): ("q1", '1', 1)
    of ("q0", '0'): ("q2", '1', 1)
    of ("q1", '1'): ("q0", '1', 1)
    of ("q1", '0'): ("q1", '1', 1)
    of ("q2", '1'): ("q2", '1', 1)
    of ("q2", '0'): ("HALT", '0', 0)
    else: ("HALT", '0', 0)

while state != "HALT":
    let symbol = if head < tape.len: tape[head] else: '0'
    let action = determineAction(state, symbol)
    state = action.state
    if head < tape.len:
        tape[head] = action.writeSymbol
    else:
        tape.add(action.writeSymbol)
    head += action.direction

    echo "State: ", state, ", Tape: ", tape.join(""), ", Head: ", head

echo "Final Tape: ", tape.join("")
