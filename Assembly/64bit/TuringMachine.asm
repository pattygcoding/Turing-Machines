section .data
    tape db "aabb  "        ; Tape with input string and extra spaces
    head dq 0               ; Head position (64-bit)
    state db 0              ; State (0 = q0, 1 = q1, 2 = q_accept, 3 = q_reject)

accept_msg db "Input is accepted by the Turing Machine.", 10
accept_msg_len equ $-accept_msg

reject_msg db "Input is rejected by the Turing Machine.", 10
reject_msg_len equ $-reject_msg

section .text
    global _start

_start:
    mov rsi, tape   ; RSI points to the tape
    mov [head], rsi ; Set the initial head position

state_q0:
    mov al, [rsi]   ; Load the current symbol under the head
    cmp al, 'a'
    je found_a_q0
    cmp al, ' '
    je accept
    jmp reject

found_a_q0:
    mov byte [rsi], 'X' ; Mark the 'a' as processed
    inc rsi             ; Move the head to the right
    jmp state_q1

state_q1:
    mov al, [rsi]   ; Load the current symbol under the head
    cmp al, 'a'
    je found_a_q1
    cmp al, 'b'
    je found_b_q1
    jmp reject

found_a_q1:
    inc rsi        ; Move the head to the right
    jmp state_q1   ; Stay in state q1

found_b_q1:
    mov byte [rsi], 'Y' ; Mark the 'b' as processed
    inc rsi             ; Move the head to the right
    jmp state_q0        ; Return to state q0 to continue processing

accept:
    ; Code to handle the accept state
    mov rax, 1            ; sys_write
    mov rdi, 1            ; file descriptor (stdout)
    mov rsi, accept_msg   ; message to print
    mov rdx, accept_msg_len
    syscall               ; Make syscall
    jmp done

reject:
    ; Code to handle the reject state
    mov rax, 1            ; sys_write
    mov rdi, 1            ; file descriptor (stdout)
    mov rsi, reject_msg   ; message to print
    mov rdx, reject_msg_len
    syscall               ; Make syscall
    jmp done

done:
    mov rax, 60           ; sys_exit
    xor rdi, rdi          ; status 0
    syscall               ; Make syscall
