package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

func main() {
    reader := bufio.NewReader(os.Stdin)
    raw, _ := reader.ReadString('\n')
    line := strings.TrimRight(raw, "\r\n")

    if strings.IndexFunc(line, func(r rune) bool { return r != '0' && r != '1' }) >= 0 {
        fmt.Println("Invalid input: expected a binary string.")
        return
    }

    // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to
    // 0 and the head moves left (carry); a 0 becomes 1 and the machine halts;
    // a blank becomes 1 (a new leading digit) and the machine halts.
    tape := []byte(line)
    head := len(tape) - 1
    for {
        symbol := byte('_')
        if head >= 0 {
            symbol = tape[head]
        }
        if symbol == '1' {
            tape[head] = '0'
            head--
        } else if symbol == '0' {
            tape[head] = '1'
            break
        } else {
            tape = append([]byte{'1'}, tape...)
            break
        }
    }

    fmt.Println("Initial tape: " + line)
    fmt.Println("Final tape: " + string(tape))
}
