class TuringMachine:
    def __init__(self, input_string):
        # Initialize the tape with input and add extra space on both sides
        self.tape = list(" " * 100 + input_string + " " * 100)
        self.head = 100  # Start at the middle of the tape
        self.state = "q0"  # Initial state

    def execute(self):
        while self.state not in ["accept", "reject"]:
            current_symbol = self.tape[self.head]

            if self.state == "q0":
                if current_symbol == 'a':
                    self.tape[self.head] = 'X'
                    self.head += 1
                    self.state = "q1"
                elif current_symbol == ' ':
                    self.state = "accept"
                else:
                    self.state = "reject"

            elif self.state == "q1":
                if current_symbol in ['a', 'X']:
                    self.head += 1
                elif current_symbol == 'b':
                    self.tape[self.head] = 'Y'
                    self.head += 1
                    self.state = "q2"
                else:
                    self.state = "reject"

            elif self.state == "q2":
                if current_symbol in ['b', 'Y']:
                    self.head += 1
                elif current_symbol == 'c':
                    self.tape[self.head] = 'Z'
                    self.head -= 1
                    self.state = "q3"
                else:
                    self.state = "reject"

            elif self.state == "q3":
                if current_symbol in ['b', 'Y']:
                    self.head -= 1
                elif current_symbol == 'X':
                    self.head += 1
                    self.state = "q0"
                else:
                    self.state = "reject"

        if self.state == "accept":
            print("Input is accepted by the Turing Machine.")
        elif self.state == "reject":
            print("Input is rejected by the Turing Machine.")

    def print_tape(self):
        print(''.join(self.tape).strip())

if __name__ == "__main__":
    input_string = "aabbcc"
    tm = TuringMachine(input_string)
    tm.execute()
    tm.print_tape()
