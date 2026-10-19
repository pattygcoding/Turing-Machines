class TuringMachine {
    var tape: [Character]
    var head: Int
    var state: String

    init(input: String) {
        self.tape = Array("  \(input)  ") // Tape with input string and extra spaces
        self.head = 2 // Start at the first character of the input
        self.state = "q0" // Initial state
    }

    func execute() {
        while state != "accept" && state != "reject" {
            let currentSymbol = tape[head]

            switch state {
            case "q0":
                if currentSymbol == "a" {
                    tape[head] = "X"
                    head += 1
                    state = "q1"
                } else if currentSymbol == " " {
                    state = "accept"
                } else {
                    state = "reject"
                }

            case "q1":
                if currentSymbol == "a" || currentSymbol == "X" {
                    head += 1
                } else if currentSymbol == "b" {
                    tape[head] = "Y"
                    head += 1
                    state = "q2"
                } else {
                    state = "reject"
                }

            case "q2":
                if currentSymbol == "b" || currentSymbol == "Y" {
                    head += 1
                } else if currentSymbol == " " {
                    state = "accept"
                } else {
                    state = "reject"
                }

            default:
                state = "reject"
            }
        }

        if state == "accept" {
            print("Input is accepted by the Turing Machine.")
        } else {
            print("Input is rejected by the Turing Machine.")
        }
    }
}

let input = "aabb"
let tm = TuringMachine(input: input)
tm.execute()
