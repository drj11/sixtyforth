BITS 64

sys_read EQU 0
sys_write EQU 1

SECTION .bss
        buf RESB 8192
        buflen EQU $-buf

SECTION .data

SECTION .text
GLOBAL _start
_start:

; This program prints rax.
        mov rax, 314592

; Iteratively divide RAX by 10, until it becomes zero.
; Pushing each remainder as we go,
; counting the number of pushes in RBX.
        mov rbx, 0
        mov rcx, 10     ; radix
div10:
        mov rdx, 0
        idiv rcx
        push rdx
        inc rbx
        test rax, rax
        jnz div10

; pop all the residues into buf
        mov rdx, buf
popit:
        pop rax
        add rax, 48
        mov [rdx], al
        inc rdx
        dec rbx
        jnz popit

; write out buf
        mov byte [rdx], 10
        sub rdx, buf    ; the buffer length
        inc rdx
        mov rdi, 1      ; stdout
        mov rsi, buf
        mov rax, sys_write
        syscall

done:
        mov rdi, 0
        mov rax, 60
        syscall
