BITS 64

sys_read EQU 0
sys_write EQU 1

SECTION .bss
        buf RESB 8192
        buflen EQU $-buf

        stack RESB 100000
        continuationstack RESB 100000

SECTION .data
program:
        DQ stdexe
        DQ LIT
        DQ 314592
        DQ DOT
        DQ NEXTWORD

ipl:    DQ stdexe
        DQ program
        DQ EXIT

LIT:
        DQ implit
SWAP:
        DQ impswap
DOT:
        DQ stdexe
        DQ LIT
        DQ 99
        DQ SWAP
        DQ restofDOT
        DQ NEXTWORD
restofDOT:
        DQ imprestofdot
EXIT:
        DQ impexit
NEXTWORD:
        DQ impnextword

SECTION .text
GLOBAL _start
_start:
        ; Initialise the model registers.
        mov r8, stack
        mov r10, continuationstack
        ; Initialising RDX and R9, so as to fake
        ; executing the Forth word IPL.
        mov rdx, program
        mov r9, ipl+16

stdexe:
        mov [r10], r9
        add r10, 8
        lea r9, [rdx+8]
next:
        mov rdx, [r9]
        add r9, 8
        mov rbp, [rdx]
        jmp rbp

;;; Machine code implementations of various Forth words.

impnextword:
        sub r10, 8
        mov r9, [r10]
        jmp next

implit:
        mov rax, [r9]
        add r9, 8
        mov [r8], rax
        add r8, 8
        jmp next

impswap:
        ; SWAP (A B -- B A)
        mov rbp, [r8-16]
        mov rdx, [r8-8]
        mov [r8-16], rdx
        mov [r8-8], rbp
        jmp next

impexit:
        mov rdi, 0
        mov rax, 60
        syscall

imprestofdot: ; ( N 99 -- )
; Observation: It is easy to calculate the least significant digit,
; by dividing by 10 and taking the remainder.
; We proceed by pushing the digits onto the stack,
; pushing the least significant first.
; This creates a stack of digits of variable length;
; we mark the beginning of the stack of digits with
; a sentinel value, which is 99 (which can't possible be a digit).

div10:
        ; LIT 10
        mov qword [r8], 10
        add r8, 8

        ; /MOD (dividend divisor -- quotient remainder)
        ; > RCX
        sub r8, 8
        mov rcx, [r8]
        ; > RAX
        sub r8, 8
        mov rax, [r8]

        mov rdx, 0
        idiv rcx
        inc rbx

        ; RAX >
        mov [r8], rax
        add r8, 8
        ; RDX >
        mov [r8], rdx
        add r8, 8
        ; end /MOD

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
        cmp rax, 99
        jz writebuf     ; break
        add rax, 48
        mov [rdx], al
        inc rdx
        jmp popit

writebuf:
; write out buf
        mov byte [rdx], 10      ; Add LF to buffer
        inc rdx
        sub rdx, buf    ; the buffer length
        mov rdi, 1      ; stdout
        mov rsi, buf
        mov rax, sys_write
        syscall
        jmp next
