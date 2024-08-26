#include <stdio.h>
#include <string.h>

#define TAPE_SIZE 100
#define HALT_STATE -1

typedef struct {
    int currentState;
    char readSymbol;
    char writeSymbol;
    char direction; // 'L' for left, 'R' for right
    int nextState;
} Transition;

int findTransition(Transition transitions[], int numTransitions, int currentState, char readSymbol) {
    for (int i = 0; i < numTransitions; i++) {
        if (transitions[i].currentState == currentState && transitions[i].readSymbol == readSymbol) {
            return i;
        }
    }
    return -1;
}

void runTuringMachine(char tape[], Transition transitions[], int numTransitions, int initialState) {
    int currentState = initialState;
    int headPosition = 0;

    while (currentState != HALT_STATE) {
        char currentSymbol = tape[headPosition];
        int transitionIndex = findTransition(transitions, numTransitions, currentState, currentSymbol);

        if (transitionIndex == -1) {
            printf("No valid transition found. Halting...\n");
            break;
        }

        Transition transition = transitions[transitionIndex];

        // Perform the transition
        tape[headPosition] = transition.writeSymbol;
        headPosition += (transition.direction == 'R') ? 1 : -1;
        currentState = transition.nextState;

        // Prevent the head from moving out of bounds
        if (headPosition < 0 || headPosition >= TAPE_SIZE) {
            printf("Tape head moved out of bounds. Halting...\n");
            break;
        }
    }
}

int main() {
    char tape[TAPE_SIZE];
    strcpy(tape, "0101010101"); // Example input

    // Define the transitions for flipping the bits
    Transition transitions[] = {
        {0, '0', '1', 'R', 0},
        {0, '1', '0', 'R', 0},
        {0, '_', '_', 'R', HALT_STATE} // Halt on blank symbol (assumes '_' as blank)
    };
    int numTransitions = sizeof(transitions) / sizeof(Transition);

    printf("Initial tape: %s\n", tape);
    runTuringMachine(tape, transitions, numTransitions, 0);
    printf("Final tape: %s\n", tape);

    return 0;
}
