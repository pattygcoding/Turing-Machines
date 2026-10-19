import java.io.BufferedReader
import java.io.InputStreamReader

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    val line = reader.readLine() ?: ""

    if (line.any { it != '0' && it != '1' }) {
        println("Invalid input: expected a binary string.")
        return
    }

    // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to
    // 0 and the head moves left (carry); a 0 becomes 1 and the machine halts;
    // a blank becomes 1 (a new leading digit) and the machine halts.
    val tape = StringBuilder(line)
    var head = tape.length - 1
    while (true) {
        val symbol = if (head >= 0) tape[head] else '_'
        if (symbol == '1') {
            tape.setCharAt(head, '0')
            head -= 1
        } else if (symbol == '0') {
            tape.setCharAt(head, '1')
            break
        } else {
            tape.insert(0, '1')
            break
        }
    }

    println("Initial tape: $line")
    println("Final tape: $tape")
}
