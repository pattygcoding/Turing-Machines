import sys

BLANK = "_"


class TuringMachine:
    def __init__(self, bits):
        # The tape starts as the input bits; the head starts on the last bit.
        self.tape = list(bits)
        self.head = len(self.tape) - 1
        self.state = "q0"

    def read(self):
        # A cell beyond the left end of the tape is blank.
        return self.tape[self.head] if self.head >= 0 else BLANK

    def write(self, symbol):
        self.tape[self.head] = symbol

    def run(self):
        # The canonical machine: a binary incrementer.  In q0 a 1 is rewritten
        # to 0 and the head moves left (carry); a 0 becomes 1 and the machine
        # halts; a blank becomes 1 (a new leading digit) and the machine halts.
        while self.state != "halt":
            symbol = self.read()
            if symbol == "1":
                self.write("0")
                self.head -= 1
            elif symbol == "0":
                self.write("1")
                self.state = "halt"
            else:
                self.tape.insert(0, "1")
                self.state = "halt"
        return "".join(self.tape)


def read_line():
    line = sys.stdin.readline()
    if line == "":
        return ""
    return line.rstrip("\n").rstrip("\r")


def main():
    line = read_line()
    if any(bit not in "01" for bit in line):
        print("Invalid input: expected a binary string.")
        return
    machine = TuringMachine(line)
    print("Initial tape: " + line)
    print("Final tape: " + machine.run())


if __name__ == "__main__":
    main()
