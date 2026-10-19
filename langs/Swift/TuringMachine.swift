import Foundation

let line = readLine() ?? ""

if line.contains(where: { $0 != "0" && $0 != "1" }) {
    print("Invalid input: expected a binary string.")
} else {
    // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to
    // 0 and the head moves left (carry); a 0 becomes 1 and the machine halts;
    // a blank becomes 1 (a new leading digit) and the machine halts.
    var tape = Array(line)
    var head = tape.count - 1
    while true {
        let symbol: Character = head >= 0 ? tape[head] : "_"
        if symbol == "1" {
            tape[head] = "0"
            head -= 1
        } else if symbol == "0" {
            tape[head] = "1"
            break
        } else {
            tape.insert("1", at: 0)
            break
        }
    }
    print("Initial tape: \(line)")
    print("Final tape: \(String(tape))")
}
