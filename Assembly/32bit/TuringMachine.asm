section .data
    tape db "aabb  " ; Tape with input string and extra spaces
    head dd 0       ; Head position
    state db 0      ; State (0 = q0, 1 = q1, 2 = q_accept, 3 = q_reject)

section .text
    global _start

_start:
    mov esi, tape   ; ESI points to the tape
    mov [head], esi ; Set the initial head position

state_q0:
    mov al, [esi]   ; Load the current symbol under the head
    cmp al, 'a'
    je found_a_q0
    cmp al, ' '
    je accept
    jmp reject

found_a_q0:
    mov byte [esi], 'X' ; Mark the 'a' as processed
    inc esi             ; Move the head to the right
    jmp state_q1

state_q1:
    mov al, [esi]   ; Load the current symbol under the head
    cmp al, 'a'
    je found_a_q1
    cmp al, 'b'
    je found_b_q1
    jmp reject

found_a_q1:
    inc esi        ; Move the head to the right
    jmp state_q1   ; Stay in state q1

found_b_q1:
    mov byte [esi], 'Y' ; Mark the 'b' as processed
    inc esi             ; Move the head to the right
    jmp state_q0        ; Return to state q0 to continue processing

accept:
    ; Code to handle the accept state
    mov eax, 4            ; sys_write
    mov ebx, 1            ; file descriptor (stdout)
    mov ecx, accept_msg
    mov edx, accept_msg_len
    int 0x80              ; Make syscall
    jmp done

reject:
    ; Code to handle the reject state
    mov eax, 4            ; sys_write
    mov ebx, 1            ; file descriptor (stdout)
    mov ecx, reject_msg
    mov edx, reject_msg_len
    int 0x80              ; Make syscall
    jmp done

done:
    mov eax, 1            ; sys_exit
    xor ebx, ebx          ; status 0
    int 0x80              ; Make syscall

section .data
accept_msg db "Input is accepted by the Turing Machine.", 10
accept_msg_len equ $-accept_msg

reject_msg db "Input is rejected by the Turing Machine.", 10
reject_msg_len equ $-reject_msg
