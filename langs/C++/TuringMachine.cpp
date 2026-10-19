#include <iostream>
#include <string>

int main() {
    std::string line;
    if (!std::getline(std::cin, line)) {
        line = "";
    }
    if (!line.empty() && line.back() == '\r') {
        line.pop_back();
    }

    for (char character : line) {
        if (character != '0' && character != '1') {
            std::cout << "Invalid input: expected a binary string.\n";
            return 0;
        }
    }

    // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to
    // 0 and the head moves left (carry); a 0 becomes 1 and the machine halts;
    // a blank becomes 1 (a new leading digit) and the machine halts.
    std::string tape = line;
    int head = static_cast<int>(tape.size()) - 1;
    for (;;) {
        char symbol = head >= 0 ? tape[head] : '_';
        if (symbol == '1') {
            tape[head] = '0';
            head -= 1;
        } else if (symbol == '0') {
            tape[head] = '1';
            break;
        } else {
            tape.insert(tape.begin(), '1');
            break;
        }
    }

    std::cout << "Initial tape: " << line << "\n";
    std::cout << "Final tape: " << tape << "\n";
    return 0;
}
