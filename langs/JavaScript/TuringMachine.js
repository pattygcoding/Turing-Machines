import fs from "node:fs";

const BLANK = "_";

class TuringMachine {
    constructor(bits) {
        // The tape starts as the input bits; the head starts on the last bit.
        this.tape = bits.split("");
        this.head = this.tape.length - 1;
        this.state = "q0";
    }

    read() {
        // A cell beyond the left end of the tape is blank.
        return this.head >= 0 ? this.tape[this.head] : BLANK;
    }

    write(symbol) {
        this.tape[this.head] = symbol;
    }

    run() {
        // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten
        // to 0 and the head moves left (carry); a 0 becomes 1 and the machine
        // halts; a blank becomes 1 (a new leading digit) and the machine halts.
        while (this.state !== "halt") {
            const symbol = this.read();
            if (symbol === "1") {
                this.write("0");
                this.head -= 1;
            } else if (symbol === "0") {
                this.write("1");
                this.state = "halt";
            } else {
                this.tape.unshift("1");
                this.state = "halt";
            }
        }
        return this.tape.join("");
    }
}

function readLine() {
    const data = fs.readFileSync(0, "utf8");
    const newline = data.indexOf("\n");
    let line = newline === -1 ? data : data.slice(0, newline);
    if (line.endsWith("\r")) {
        line = line.slice(0, -1);
    }
    return line;
}

function main() {
    const line = readLine();
    if ([...line].some((bit) => bit !== "0" && bit !== "1")) {
        console.log("Invalid input: expected a binary string.");
        return;
    }
    const machine = new TuringMachine(line);
    console.log("Initial tape: " + line);
    console.log("Final tape: " + machine.run());
}

main();
