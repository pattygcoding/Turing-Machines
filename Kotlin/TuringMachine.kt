class TuringMachine(input: String) {
    private val tape = input.toCharArray() + "  ".toCharArray()
    private var head = 0
    private var state = "q0"

    fun execute() {
        while (state != "accept" && state != "reject") {
            val currentSymbol = tape[head]

            when (state) {
                "q0" -> when (currentSymbol) {
                    'a' -> {
                        tape[head] = 'X'
                        head++
                        state = "q1"
                    }
                    ' ' -> state = "accept"
                    else -> state = "reject"
                }

                "q1" -> when (currentSymbol) {
                    'a', 'X' -> head++
                    'b' -> {
                        tape[head] = 'Y'
                        head++
                        state = "q2"
                    }
                    else -> state = "reject"
                }

                "q2" -> when (currentSymbol) {
                    'b', 'Y' -> head++
                    ' ' -> state = "accept"
                    else -> state = "reject"
                }
            }
        }

        if (state == "accept") {
            println("Input is accepted by the Turing Machine.")
        } else {
            println("Input is rejected by the Turing Machine.")
        }
    }
}

fun main() {
    val input = "aabb"
    val tm = TuringMachine(input)
    tm.execute()
}
