; The canonical machine: a binary incrementer, written directly against the
; Win64 API (no C runtime), so nasm + gcc can link it without a CRT.

    default rel

    section .data
initial_label:      db "Initial tape: "
initial_label_len   equ $ - initial_label
final_label:        db "Final tape: "
final_label_len     equ $ - final_label
invalid_message:    db "Invalid input: expected a binary string.", 10
invalid_len         equ $ - invalid_message
newline:            db 10

    section .bss
input_buffer:       resb 512
tape_buffer:        resb 1024
output_buffer:      resb 4096
bytes_io:           resq 1

    section .text
    extern GetStdHandle
    extern ReadFile
    extern WriteFile
    global main

; Copy rcx bytes from rsi into output_buffer + r13.
copy_bytes:
    test rcx, rcx
    jz .copy_done
.copy_loop:
    mov al, [rsi]
    mov [rdi + r13], al
    inc rsi
    inc r13
    dec rcx
    jnz .copy_loop
.copy_done:
    ret

main:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 40

    lea rsi, [input_buffer]
    mov byte [rsi], 0

    mov ecx, -10
    call GetStdHandle
    mov r12, rax

    mov rcx, r12
    lea rdx, [input_buffer]
    mov r8d, 511
    lea r9, [bytes_io]
    mov qword [rsp + 32], 0
    call ReadFile

    mov r13, [bytes_io]

    xor r14, r14
.find_newline:
    cmp r14, r13
    jae .strip_cr
    mov al, [rsi + r14]
    cmp al, 10
    je .strip_cr
    inc r14
    jmp .find_newline
.strip_cr:
    test r14, r14
    jz .validate
    mov al, [rsi + r14 - 1]
    cmp al, 13
    jne .validate
    dec r14

.validate:
    xor r15, r15
.validate_loop:
    cmp r15, r14
    jae .run
    mov al, [rsi + r15]
    cmp al, '0'
    je .validate_next
    cmp al, '1'
    je .validate_next
    jmp .invalid
.validate_next:
    inc r15
    jmp .validate_loop

.invalid:
    mov ecx, -11
    call GetStdHandle
    mov rcx, rax
    lea rdx, [invalid_message]
    mov r8d, invalid_len
    lea r9, [bytes_io]
    mov qword [rsp + 32], 0
    call WriteFile
    jmp .done

.run:
    lea rbx, [tape_buffer]
    xor r15, r15
.copy_tape:
    cmp r15, r14
    jae .carry
    mov al, [rsi + r15]
    mov [rbx + r15 + 1], al
    inc r15
    jmp .copy_tape

.carry:
    mov r15, r14
.carry_loop:
    test r15, r15
    jz .prepend
    mov al, [rbx + r15]
    cmp al, '1'
    jne .write_one
    mov byte [rbx + r15], '0'
    dec r15
    jmp .carry_loop
.write_one:
    mov byte [rbx + r15], '1'
    mov r12, 1
    mov r15, r14
    jmp .emit
.prepend:
    mov byte [rbx], '1'
    xor r12, r12
    lea r15, [r14 + 1]

.emit:
    lea rdi, [output_buffer]
    xor r13, r13

    lea rsi, [initial_label]
    mov rcx, initial_label_len
    call copy_bytes

    lea rsi, [input_buffer]
    mov rcx, r14
    call copy_bytes

    lea rsi, [newline]
    mov rcx, 1
    call copy_bytes

    lea rsi, [final_label]
    mov rcx, final_label_len
    call copy_bytes

    lea rsi, [rbx + r12]
    mov rcx, r15
    call copy_bytes

    lea rsi, [newline]
    mov rcx, 1
    call copy_bytes

    mov ecx, -11
    call GetStdHandle
    mov rcx, rax
    lea rdx, [output_buffer]
    mov r8, r13
    lea r9, [bytes_io]
    mov qword [rsp + 32], 0
    call WriteFile

.done:
    xor eax, eax
    add rsp, 40
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
