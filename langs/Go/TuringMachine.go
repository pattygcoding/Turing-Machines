package main

import (
	"fmt"
)

// Transition defines the structure for each state transition.
type Transition struct {
	writeSymbol  string
	moveDirection string
	nextState     string
}

// TuringMachine defines the structure of the Turing machine.
type TuringMachine struct {
	tape             []string
	headPosition     int
	currentState     string
	transitionTable  map[string]map[string]Transition
}

// NewTuringMachine initializes a new Turing machine.
func NewTuringMachine(tape []string, startState string, transitionTable map[string]map[string]Transition) *TuringMachine {
	return &TuringMachine{
		tape:             tape,
		headPosition:     0,
		currentState:     startState,
		transitionTable:  transitionTable,
	}
}

// Run executes the Turing machine until it halts.
func (tm *TuringMachine) Run() {
	for tm.currentState != "HALT" {
		currentSymbol := tm.tape[tm.headPosition]
		transition, ok := tm.transitionTable[tm.currentState][currentSymbol]

		if !ok {
			fmt.Println("No transition found. Halting.")
			break
		}

		// Write the new symbol to the tape
		tm.tape[tm.headPosition] = transition.writeSymbol

		// Move the head
		if transition.moveDirection == "R" {
			tm.headPosition++
		} else if transition.moveDirection == "L" {
			tm.headPosition--
		}

		// Update the state
		tm.currentState = transition.nextState

		// If the head goes off the tape, extend the tape
		if tm.headPosition < 0 {
			tm.tape = append([]string{"_"}, tm.tape...)
			tm.headPosition = 0
		} else if tm.headPosition >= len(tm.tape) {
			tm.tape = append(tm.tape, "_")
		}

		// Print the current tape state
		fmt.Println("Tape: ", tm.tape)
		fmt.Println("Head Position: ", tm.headPosition)
		fmt.Println("Current State: ", tm.currentState)
		fmt.Println("---")
	}
}

func main() {
	// Define the tape
	tape := []string{"1", "0", "1", "_", "_"}

	// Define the transition table
	transitionTable := map[string]map[string]Transition{
		"q0": {
			"1": {writeSymbol: "1", moveDirection: "R", nextState: "q0"},
			"0": {writeSymbol: "0", moveDirection: "R", nextState: "q1"},
			"_": {writeSymbol: "_", moveDirection: "L", nextState: "HALT"},
		},
		"q1": {
			"1": {writeSymbol: "0", moveDirection: "L", nextState: "q1"},
			"0": {writeSymbol: "1", moveDirection: "L", nextState: "q0"},
			"_": {writeSymbol: "_", moveDirection: "R", nextState: "HALT"},
		},
	}

	// Create the Turing machine
	tm := NewTuringMachine(tape, "q0", transitionTable)

	// Run the Turing machine
	tm.Run()
}
