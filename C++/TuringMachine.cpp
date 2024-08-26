#include <iostream>
#include <vector>
#include <string>

#define TAPE_SIZE 100
#define HALT_STATE -1

class Transition {
public:
    int currentState;
    char readSymbol;
    char writeSymbol;
    char direction; // 'L' for left, 'R' for right
    int nextState;

    Transition(int currState, char readSym, char writeSym, char dir, int nextState)
        : currentState(currState), readSymbol(readSym), writeSymbol(writeSym), direction(dir), nextState(nextState) {}
};

int findTransition(const std::vector<Transition>& transitions, int currentState, char readSymbol) {
    for (size_t i = 0; i < transitions.size(); ++i) {
        if (transitions[i].currentState == currentState && transitions[i].readSymbol == readSymbol) {
            return i;
        }
    }
    return -1;
}

void runTuringMachine(std::string& tape, const std::vector<Transition>& transitions, int initialState) {
    int currentState = initialState;
    int headPosition = 0;

    while (currentState != HALT_STATE) {
        char currentSymbol = tape[headPosition];
        int transitionIndex = findTransition(transitions, currentState, currentSymbol);

        if (transitionIndex == -1) {
            std::cout << "No valid transition found. Halting...\n";
            break;
        }

        const Transition& transition = transitions[transitionIndex];

        // Perform the transition
        tape[headPosition] = transition.writeSymbol;
        headPosition += (transition.direction == 'R') ? 1 : -1;
        currentState = transition.nextState;

        // Prevent the head from moving out of bounds
        if (headPosition < 0 || headPosition >= TAPE_SIZE) {
            std::cout << "Tape head moved out of bounds. Halting...\n";
            break;
        }
    }
}

int main() {
    std::string tape = "0101010101"; // Example input

    // Define the transitions for flipping the bits
    std::vector<Transition> transitions = {
        Transition(0, '0', '1', 'R', 0),
        Transition(0, '1', '0', 'R', 0),
        Transition(0, '_', '_', 'R', HALT_STATE) // Halt on blank symbol (assumes '_' as blank)
    };

    std::cout << "Initial tape: " << tape << "\n";
    runTuringMachine(tape, transitions, 0);
    std::cout << "Final tape: " << tape << "\n";

    return 0;
}
