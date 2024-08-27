import 'dart:collection';

class TuringMachine {
  List<String> tape;
  int headPosition;
  String currentState;
  Map<String, Map<String, Transition>> transitionFunction;

  TuringMachine({
    required List<String> tape,
    required String startState,
    required this.transitionFunction,
  })  : this.tape = List.from(tape),
        this.headPosition = 0,
        this.currentState = startState;

  void run() {
    while (currentState != 'HALT') {
      String currentSymbol = tape[headPosition];
      Transition? transition =
          transitionFunction[currentState]?[currentSymbol];

      if (transition == null) {
        print('No transition found. Halting.');
        break;
      }

      // Write the new symbol to the tape
      tape[headPosition] = transition.writeSymbol;

      // Move the head
      headPosition += transition.moveDirection == 'R' ? 1 : -1;

      // Update the state
      currentState = transition.nextState;

      // If the head goes off the tape, extend the tape
      if (headPosition < 0) {
        tape.insert(0, '_');
        headPosition = 0;
      } else if (headPosition >= tape.length) {
        tape.add('_');
      }

      // Print the current tape state
      print('Tape: ${tape.join('')}');
      print('Head Position: $headPosition');
      print('Current State: $currentState');
      print('---');
    }
  }
}

class Transition {
  final String writeSymbol;
  final String moveDirection; // 'L' for left, 'R' for right
  final String nextState;

  Transition({
    required this.writeSymbol,
    required this.moveDirection,
    required this.nextState,
  });
}

void main() {
  // Define the tape
  List<String> tape = ['1', '0', '1', '_', '_'];

  // Define the transition function
  Map<String, Map<String, Transition>> transitionFunction = {
    'q0': {
      '1': Transition(writeSymbol: '1', moveDirection: 'R', nextState: 'q0'),
      '0': Transition(writeSymbol: '0', moveDirection: 'R', nextState: 'q1'),
      '_': Transition(writeSymbol: '_', moveDirection: 'L', nextState: 'HALT'),
    },
    'q1': {
      '1': Transition(writeSymbol: '0', moveDirection: 'L', nextState: 'q1'),
      '0': Transition(writeSymbol: '1', moveDirection: 'L', nextState: 'q0'),
      '_': Transition(writeSymbol: '_', moveDirection: 'R', nextState: 'HALT'),
    },
  };

  // Create the Turing machine
  TuringMachine tm = TuringMachine(
    tape: tape,
    startState: 'q0',
    transitionFunction: transitionFunction,
  );

  // Run the Turing machine
  tm.run();
}
