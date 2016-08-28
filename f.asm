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
ZEROBRANCH:
        DQ impzerobranch
SWAP:
        DQ impswap
DUP:
        DQ impdup
DROP:
        DQ impdrop
NEQ0:
        DQ impneq0
DOT:
; Observation: It is easy to calculate the least significant digit,
; by dividing by 10 and taking the remainder.
; We proceed by pushing the digits onto the stack,
; pushing the least significant first.
; This creates a stack of digits of variable length;
; we mark the beginning of the stack of digits with
; a sentinel value, which is 99 (which can't possible be a digit).
        DQ stdexe
        DQ LIT
        DQ 99
        DQ SWAP
l1:     DQ LIT
        DQ 10
        DQ DIVMOD       ; -- Q R
        DQ SWAP         ; -- R Q
        DQ DUP          ; -- R Q Q
        DQ NEQ0         ; -- R Q B
        DQ ZEROBRANCH   ; -- R Q
        DQ -8
        DQ DROP
        DQ restofDOT
        DQ NEXTWORD
restofDOT:
        DQ imprestofdot
DIVMOD:
        DQ impdivmod
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

impzerobranch:
        ; read the next word as a relative offset;
        ; pop the TOS and test it;
        ; if it is 0 then branch by adding the offset
        ; to CODEPOINTER.
        mov rbx, [r9]
        add r9, 8
        mov rax, [r8]
        sub r8, 8
        test rax, rax
        jnz next
        lea r9, [r9 + 8*rbx]
        jmp next

impswap:
        ; SWAP (A B -- B A)
        mov rbp, [r8-16]
        mov rdx, [r8-8]
        mov [r8-16], rdx
        mov [r8-8], rbp
        jmp next
impdup:
        ; DUP (A -- A A)
        mov rdx, [r8-8]
        mov [r8], rdx
        add r8, 8
        jmp next
impdrop:
        ; DROP (A -- )
        sub r8, 8
        jmp next
impneq0:
        ; NEQ0 (A -- Bool)
        ; Result is 0 (TRUE) if A != 0;
        ; Result is -1 (FALSE) otherwise.
        mov rax, [r8-8]
        sub rax, 1      ; is-zero now in Carry flag
        sbb rax, rax
        mov [r8], rax
        jmp next

impexit:
        mov rdi, 0
        mov rax, 60
        syscall

impdivmod:      ; /MOD (dividend divisor -- quotient remainder)
        ; > RCX
        sub r8, 8
        mov rcx, [r8]
        ; > RAX
        sub r8, 8
        mov rax, [r8]

        mov rdx, 0
        idiv rcx

        ; RAX >
        mov [r8], rax
        add r8, 8
        ; RDX >
        mov [r8], rdx
        add r8, 8
        jmp next

imprestofdot: ; ( 99 dN ... d1 -- )

; pop all the residues into buf
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
