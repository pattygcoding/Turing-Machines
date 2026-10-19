object TuringMachine extends App {
  // Define the tape (with some initial input)
  var tape: Array[Char] = "1101".toCharArray // Initial input
  var head: Int = 0                          // Initial head position
  var state: String = "q0"                   // Initial state

  // Define the transition table
  val transitions: Map[(String, Char), (String, Char, Int)] = Map(
    ("q0", '1') -> ("q1", '1', 1),  // On state q0 and reading '1' -> move to q1, write '1', move right
    ("q0", '0') -> ("q2", '1', 1),  // On state q0 and reading '0' -> move to q2, write '1', move right
    ("q1", '1') -> ("q0", '1', 1),  // On state q1 and reading '1' -> move to q0, write '1', move right
    ("q1", '0') -> ("q1", '1', 1),  // On state q1 and reading '0' -> stay in q1, write '1', move right
    ("q2", '1') -> ("q2", '1', 1),  // On state q2 and reading '1' -> stay in q2, write '1', move right
    ("q2", '0') -> ("HALT", '0', 0) // On state q2 and reading '0' -> halt
  )

  // Function to run the Turing machine
  def runMachine(): Unit = {
    while (true) {
      // Get the current symbol on the tape
      val symbol: Char = if (head < tape.length) tape(head) else '0'

      // Determine the action based on the current state and symbol
      transitions.get((state, symbol)) match {
        case Some((newState, newSymbol, direction)) =>
          state = newState                   // Move to the new state
          if (head < tape.length) tape(head) = newSymbol // Write the new symbol

          head += direction                   // Move the head (left: -1, right: +1)
          
          // Output the current state of the tape and head position
          println(s"State: $state, Tape: ${tape.mkString}, Head: $head")

          // Halt if we reach the HALT state
          if (state == "HALT") return

        case None =>
          // No transition defined; halt the machine
          println("No valid transition found. Halting.")
          return
      }
    }
  }

  // Start the Turing machine
  runMachine()

  // Output the final tape
  println(s"Final Tape: ${tape.mkString}")
}
