import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class TuringMachine {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String line = reader.readLine();
        if (line == null) {
            line = "";
        }

        for (int index = 0; index < line.length(); index++) {
            char character = line.charAt(index);
            if (character != '0' && character != '1') {
                System.out.println("Invalid input: expected a binary string.");
                return;
            }
        }

        // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten
        // to 0 and the head moves left (carry); a 0 becomes 1 and the machine
        // halts; a blank becomes 1 (a new leading digit) and the machine halts.
        StringBuilder tape = new StringBuilder(line);
        int head = tape.length() - 1;
        while (true) {
            char symbol = head >= 0 ? tape.charAt(head) : '_';
            if (symbol == '1') {
                tape.setCharAt(head, '0');
                head -= 1;
            } else if (symbol == '0') {
                tape.setCharAt(head, '1');
                break;
            } else {
                tape.insert(0, '1');
                break;
            }
        }

        System.out.println("Initial tape: " + line);
        System.out.println("Final tape: " + tape);
    }
}
