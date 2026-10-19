#!/bin/bash

# Define the tape with some initial input
tape=("1" "1" "0" "1")  # Initial input
head=0                  # Initial head position
state="q0"              # Initial state

# Function to determine the action based on the current state and symbol
determine_action() {
    local state_symbol="$1_$2"
    case $state_symbol in
        "q0_1") echo "q1 1 1" ;;
        "q0_0") echo "q2 1 1" ;;
        "q1_1") echo "q0 1 1" ;;
        "q1_0") echo "q1 1 1" ;;
        "q2_1") echo "q2 1 1" ;;
        "q2_0") echo "HALT 0 0" ;;
        *) echo "HALT 0 0" ;;
    esac
}

# Main loop to run the Turing machine
while true; do
    # Get the current symbol on the tape
    symbol="${tape[$head]:-0}"

    # Determine the action
    action=$(determine_action "$state" "$symbol")
    read -r state write_symbol direction <<< "$action"

    # Apply the action
    tape[$head]=$write_symbol
    ((head += direction))

    # Output the current state of the tape and head position
    echo "State: $state, Tape: ${tape[*]}, Head: $head"

    # Halt if the state is HALT
    if [ "$state" == "HALT" ]; then
        break
    fi
done

# Output the final tape
echo "Final Tape: ${tape[*]}"
