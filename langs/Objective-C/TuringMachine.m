#import <stdio.h>
#import <string.h>

#define TAPE_SIZE 512

int main(void) {
    char line[TAPE_SIZE];
    if (fgets(line, (int)sizeof(line), stdin) == NULL) {
        line[0] = '\0';
    }

    size_t length = strlen(line);
    while (length > 0 && (line[length - 1] == '\n' || line[length - 1] == '\r')) {
        line[--length] = '\0';
    }

    for (size_t index = 0; index < length; index++) {
        if (line[index] != '0' && line[index] != '1') {
            printf("Invalid input: expected a binary string.\n");
            return 0;
        }
    }

    // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to
    // 0 and the head moves left (carry); a 0 becomes 1 and the machine halts;
    // a blank becomes 1 (a new leading digit) and the machine halts.
    char tape[TAPE_SIZE];
    memcpy(tape, line, length + 1);
    long head = (long)length - 1;
    for (;;) {
        char symbol = head >= 0 ? tape[head] : '_';
        if (symbol == '1') {
            tape[head] = '0';
            head -= 1;
        } else if (symbol == '0') {
            tape[head] = '1';
            break;
        } else {
            memmove(tape + 1, tape, strlen(tape) + 1);
            tape[0] = '1';
            break;
        }
    }

    printf("Initial tape: %s\n", line);
    printf("Final tape: %s\n", tape);
    return 0;
}
