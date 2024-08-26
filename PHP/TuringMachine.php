<?php
class TuringMachine {
    private $tape;
    private $head;
    private $state;

    public function __construct($input) {
        // Initialize the tape with input and extra spaces on both sides
        $this->tape = str_split("  " . $input . "  ");
        $this->head = 2; // Start at the first character of the input
        $this->state = "q0"; // Initial state
    }

    public function execute() {
        while ($this->state !== "accept" && $this->state !== "reject") {
            $currentSymbol = $this->tape[$this->head];

            switch ($this->state) {
                case "q0":
                    if ($currentSymbol === 'a') {
                        $this->tape[$this->head] = 'X';
                        $this->head++;
                        $this->state = "q1";
                    } elseif ($currentSymbol === ' ') {
                        $this->state = "accept";
                    } else {
                        $this->state = "reject";
                    }
                    break;

                case "q1":
                    if ($currentSymbol === 'a' || $currentSymbol === 'X') {
                        $this->head++;
                    } elseif ($currentSymbol === 'b') {
                        $this->tape[$this->head] = 'Y';
                        $this->head++;
                        $this->state = "q2";
                    } else {
                        $this->state = "reject";
                    }
                    break;

                case "q2":
                    if ($currentSymbol === 'b' || $currentSymbol === 'Y') {
                        $this->head++;
                    } elseif ($currentSymbol === ' ') {
                        $this->state = "accept";
                    } else {
                        $this->state = "reject";
                    }
                    break;
            }
        }

        if ($this->state === "accept") {
            echo "Input is accepted by the Turing Machine.\n";
        } else {
            echo "Input is rejected by the Turing Machine.\n";
        }
    }
}

$input = "aabb";
$tm = new TuringMachine($input);
$tm->execute();
