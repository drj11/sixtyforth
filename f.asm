BITS 64

sys_read EQU 0
sys_write EQU 1

SECTION .bss
        buf RESB 8192
        buflen EQU $-buf
        stack RESB 100000

SECTION .data

SECTION .text
GLOBAL _start
_start:
        mov r8, stack

; This program prints rax.

; LIT 314592
        mov rax, 314592
        mov [r8], rax
        add r8, 8

; Iteratively divide TOS by 10, until it becomes zero.
; Pushing each remainder as we go,
; counting the number of pushes in RBX.
        mov rbx, 0
        mov rcx, 10     ; radix
div10:
        ; 10DIV (DIVIDEND -- QUOTIENT)
        mov rdx, 0
        mov rax, [r8-8]
        idiv rcx
        inc rbx
        mov [r8-8], rax

        ; ( -- RDX)
        mov [r8], rdx
        add r8, 8

        ; SWAP (A B -- B A)
        mov rbp, [r8-16]
        mov rdx, [r8-8]
        mov [r8-16], rdx
        mov [r8-8], rbp

        ; branch if TOS not 0
        mov rax, [r8-8]
        test rax, rax
        jnz div10

; pop all the residues into buf
        ; DROP (A -- )
        sub r8, 8

        mov rdx, buf
popit:
        sub r8, 8
        mov rax, [r8]
        add rax, 48
        mov [rdx], al
        inc rdx
        dec rbx
        jnz popit

; write out buf
        mov byte [rdx], 10      ; Add LF to buffer
        inc rdx
        sub rdx, buf    ; the buffer length
        mov rdi, 1      ; stdout
        mov rsi, buf
        mov rax, sys_write
        syscall

done:
        mov rdi, 0
        mov rax, 60
        syscall
