class TuringMachine {
    constructor(input) {
        this.tape = `  ${input}  `.split(''); // Tape with input string and extra spaces
        this.head = 2; // Start at the first character of the input
        this.state = 'q0'; // Initial state
    }

    execute() {
        while (this.state !== 'accept' && this.state !== 'reject') {
            const currentSymbol = this.tape[this.head];

            switch (this.state) {
                case 'q0':
                    if (currentSymbol === 'a') {
                        this.tape[this.head] = 'X';
                        this.head++;
                        this.state = 'q1';
                    } else if (currentSymbol === ' ') {
                        this.state = 'accept';
                    } else {
                        this.state = 'reject';
                    }
                    break;

                case 'q1':
                    if (currentSymbol === 'a' || currentSymbol === 'X') {
                        this.head++;
                    } else if (currentSymbol === 'b') {
                        this.tape[this.head] = 'Y';
                        this.head++;
                        this.state = 'q2';
                    } else {
                        this.state = 'reject';
                    }
                    break;

                case 'q2':
                    if (currentSymbol === 'b' || currentSymbol === 'Y') {
                        this.head++;
                    } else if (currentSymbol === ' ') {
                        this.state = 'accept';
                    } else {
                        this.state = 'reject';
                    }
                    break;
            }
        }

        if (this.state === 'accept') {
            console.log('Input is accepted by the Turing Machine.');
        } else {
            console.log('Input is rejected by the Turing Machine.');
        }
    }
}

const input = 'aabb';
const tm = new TuringMachine(input);
tm.execute();
