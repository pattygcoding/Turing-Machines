import java.util.Arrays;

class TuringMachine {
    private char[] tape;
    private int head;
    private String state;

    public TuringMachine(String input) {
        // Initialize the tape with the input and extra space on both sides
        tape = new char[300];
        Arrays.fill(tape, ' ');
        System.arraycopy(input.toCharArray(), 0, tape, 100, input.length());
        head = 100; // Start at the middle of the tape
        state = "q0"; // Initial state
    }

    public void execute() {
        while (!state.equals("accept") && !state.equals("reject")) {
            char currentSymbol = tape[head];
            switch (state) {
                case "q0":
                    if (currentSymbol == 'a') {
                        tape[head] = 'X';
                        head++;
                        state = "q1";
                    } else if (currentSymbol == ' ') {
                        state = "q_accept";
                    } else {
                        state = "q_reject";
                    }
                    break;

                case "q1":
                    if (currentSymbol == 'a' || currentSymbol == 'X') {
                        head++;
                    } else if (currentSymbol == 'b') {
                        tape[head] = 'Y';
                        head++;
                        state = "q2";
                    } else {
                        state = "q_reject";
                    }
                    break;

                case "q2":
                    if (currentSymbol == 'b' || currentSymbol == 'Y') {
                        head++;
                    } else if (currentSymbol == 'c') {
                        tape[head] = 'Z';
                        head--;
                        state = "q3";
                    } else {
                        state = "q_reject";
                    }
                    break;

                case "q3":
                    if (currentSymbol == 'b' || currentSymbol == 'Y') {
                        head--;
                    } else if (currentSymbol == 'X') {
                        head++;
                        state = "q0";
                    } else {
                        state = "q_reject";
                    }
                    break;

                case "q_accept":
                    System.out.println("Input is accepted by the Turing Machine.");
                    state = "accept";
                    break;

                case "q_reject":
                    System.out.println("Input is rejected by the Turing Machine.");
                    state = "reject";
                    break;

                default:
                    state = "q_reject";
            }
        }
    }

    public void printTape() {
        System.out.println(new String(tape).trim());
    }

    public static void main(String[] args) {
        String input = "aabbcc";
        TuringMachine tm = new TuringMachine(input);
        tm.execute();
        tm.printTape();
    }
}
