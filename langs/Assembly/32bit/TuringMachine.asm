; The canonical machine: a binary incrementer, written directly against the
; Win32 API (no C runtime), so nasm -f win32 + a 32-bit MinGW can link it.

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
bytes_io:           resd 1

    section .text
    extern _GetStdHandle@4
    extern _ReadFile@20
    extern _WriteFile@20
    global _main

; Copy ecx bytes from esi into output_buffer + edx (edi is the buffer base).
copy_bytes:
    test ecx, ecx
    jz .copy_done
.copy_loop:
    mov al, [esi]
    mov [edi + edx], al
    inc esi
    inc edx
    dec ecx
    jnz .copy_loop
.copy_done:
    ret

_main:
    push ebp
    mov ebp, esp
    push ebx
    push esi
    push edi
    sub esp, 12                  ; [ebp-16] start, [ebp-20] final len, [ebp-24] line len

    push dword -10
    call _GetStdHandle@4
    mov ebx, eax

    lea esi, [input_buffer]
    mov byte [esi], 0

    push dword 0
    push bytes_io
    push dword 511
    push esi
    push ebx
    call _ReadFile@20

    mov ecx, [bytes_io]
    xor edx, edx
.find_newline:
    cmp edx, ecx
    jae .strip_cr
    mov al, [esi + edx]
    cmp al, 10
    je .strip_cr
    inc edx
    jmp .find_newline
.strip_cr:
    test edx, edx
    jz .validate
    mov al, [esi + edx - 1]
    cmp al, 13
    jne .validate
    dec edx

.validate:
    mov [ebp - 24], edx
    xor ecx, ecx
.validate_loop:
    cmp ecx, edx
    jae .run
    mov al, [esi + ecx]
    cmp al, '0'
    je .validate_next
    cmp al, '1'
    je .validate_next
    jmp .invalid
.validate_next:
    inc ecx
    jmp .validate_loop

.invalid:
    push dword -11
    call _GetStdHandle@4
    mov ebx, eax
    push dword 0
    push bytes_io
    push dword invalid_len
    push invalid_message
    push ebx
    call _WriteFile@20
    jmp .done

.run:
    lea edi, [tape_buffer]
    xor ecx, ecx
.copy_tape:
    cmp ecx, edx
    jae .carry
    mov al, [esi + ecx]
    mov [edi + ecx + 1], al
    inc ecx
    jmp .copy_tape

.carry:
    mov ecx, edx
.carry_loop:
    test ecx, ecx
    jz .prepend
    mov al, [edi + ecx]
    cmp al, '1'
    jne .write_one
    mov byte [edi + ecx], '0'
    dec ecx
    jmp .carry_loop
.write_one:
    mov byte [edi + ecx], '1'
    mov dword [ebp - 16], 1
    mov [ebp - 20], edx
    jmp .emit
.prepend:
    mov byte [edi], '1'
    mov dword [ebp - 16], 0
    mov eax, edx
    inc eax
    mov [ebp - 20], eax

.emit:
    lea edi, [output_buffer]
    xor edx, edx

    lea esi, [initial_label]
    mov ecx, initial_label_len
    call copy_bytes

    lea esi, [input_buffer]
    mov ecx, [ebp - 24]
    call copy_bytes

    lea esi, [newline]
    mov ecx, 1
    call copy_bytes

    lea esi, [final_label]
    mov ecx, final_label_len
    call copy_bytes

    mov esi, tape_buffer
    add esi, [ebp - 16]
    mov ecx, [ebp - 20]
    call copy_bytes

    lea esi, [newline]
    mov ecx, 1
    call copy_bytes

    push dword -11
    call _GetStdHandle@4
    mov ebx, eax
    push dword 0
    push bytes_io
    push edx
    push edi
    push ebx
    call _WriteFile@20

.done:
    xor eax, eax
    add esp, 12
    pop edi
    pop esi
    pop ebx
    pop ebp
    ret
