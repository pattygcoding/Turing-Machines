class TuringMachine {
    String[] tape
    int head
    String state

    TuringMachine(String input) {
        // Initialize the tape with the input and extra spaces
        this.tape = ("  " + input + "  ").toCharArray()
        this.head = 2 // Start at the first character of the input
        this.state = "q0" // Initial state
    }

    void execute() {
        while (state != "accept" && state != "reject") {
            char currentSymbol = tape[head]

            switch (state) {
                case "q0":
                    if (currentSymbol == 'a') {
                        tape[head] = 'X'
                        head++
                        state = "q1"
                    } else if (currentSymbol == ' ') {
                        state = "accept"
                    } else {
                        state = "reject"
                    }
                    break

                case "q1":
                    if (currentSymbol == 'a' || currentSymbol == 'X') {
                        head++
                    } else if (currentSymbol == 'b') {
                        tape[head] = 'Y'
                        head++
                        state = "q2"
                    } else {
                        state = "reject"
                    }
                    break

                case "q2":
                    if (currentSymbol == 'b' || currentSymbol == 'Y') {
                        head++
                    } else if (currentSymbol == ' ') {
                        state = "accept"
                    } else {
                        state = "reject"
                    }
                    break
            }
        }

        if (state == "accept") {
            println "Input is accepted by the Turing Machine."
        } else {
            println "Input is rejected by the Turing Machine."
        }
    }
}

def input = "aabb"
def tm = new TuringMachine(input)
tm.execute()
