# Initialize the tape with some input
tape <- c("1", "1", "0", "1")  # Initial tape content
tape <- as.character(tape)
head <- 1                      # Initial head position
state <- "q0"                  # Initial state

# Define the transition table
transitions <- list(
  q0_1 = list(state = "q1", write = "1", move = 1),  # On state q0 and reading 1 -> move to q1, write 1, move right
  q0_0 = list(state = "q2", write = "1", move = 1),  # On state q0 and reading 0 -> move to q2, write 1, move right
  q1_1 = list(state = "q0", write = "1", move = 1),  # On state q1 and reading 1 -> move to q0, write 1, move right
  q1_0 = list(state = "q1", write = "1", move = 1),  # On state q1 and reading 0 -> stay in q1, write 1, move right
  q2_1 = list(state = "q2", write = "1", move = 1),  # On state q2 and reading 1 -> stay in q2, write 1, move right
  q2_0 = list(state = "HALT", write = "0", move = 0) # On state q2 and reading 0 -> halt
)

# Function to run the Turing machine
run_machine <- function() {
  repeat {
    # Get the current symbol on the tape
    symbol <- ifelse(head <= length(tape), tape[head], "0")  # Default to '0' if beyond tape bounds
    
    # Create a key for the transition lookup
    transition_key <- paste(state, symbol, sep = "_")
    
    # Determine the action based on the current state and symbol
    action <- transitions[[transition_key]]
    
    # If no action is defined, halt the machine
    if (is.null(action)) {
      cat("No valid transition found. Halting.\n")
      break
    }
    
    # Apply the action
    state <<- action$state                # Move to the new state
    if (head <= length(tape)) {
      tape[head] <<- action$write         # Write the new symbol
    } else {
      tape <<- c(tape, action$write)      # Extend the tape if necessary
    }
    head <<- head + action$move           # Move the head (left: -1, right: +1)
    
    # Output the current state of the tape and head position
    cat("State:", state, ", Tape:", paste(tape, collapse = ""), ", Head:", head, "\n")
    
    # Halt if we reach the HALT state
    if (state == "HALT") {
      break
    }
  }
  
  # Output the final tape
  cat("Final Tape:", paste(tape, collapse = ""), "\n")
}

# Start the Turing machine
run_machine()
