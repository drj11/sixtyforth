BITS 64

sys_read EQU 0
sys_write EQU 1

SECTION .bss
        buf RESB 8192
        buflen EQU $-buf

        lexbuf RESB 8192        ; buffer for lexing
                                ; (see lexptr and lexend)
        lexbufend EQU $

        wordbuf RESB 8192

        stack RESB 100000
        continuationstack RESB 100000

SECTION .data
lexptr DQ lexbuf        ; pointer to next valid byte in lexbuf
lexend DQ lexbuf        ; pointer to limit of valid bytes in lexbuf

prompt DB '> '
promptlen EQU $-prompt

; strings for dictionary
strdot: DB 'dot'
stradd: DB 'add'

; Start of Dictionary
        DQ 0
.dot    DQ strdot
        DQ 3
        DQ DOT
        DQ .dot
.add    DQ stradd
        DQ 3
        DQ ADD
        DQ .add
DICT:   DQ $-8

; read loop should be something like:
; LEX1: lex single word from input: creates a string.
; FIND: To convert from string to DICT entry.
; EXECUTE

program:
        DQ stdexe
.l:     DQ QPROMPT
        DQ LEX1
        DQ FIND
        DQ EXECUTE
        DQ BRANCH
        DQ -(($-.l)/8)-1
        DQ NEXTWORD     ; Never executed

ipl:    DQ stdexe
        DQ program
        DQ EXIT

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
.10div: DQ LIT
        DQ 10
        DQ DIVMOD       ; -- Q R
        DQ SWAP         ; -- R Q
        DQ DUP          ; -- R Q Q
        DQ EQ0          ; -- R Q flag
        DQ ZEROBRANCH   ; -- R Q
        DQ -(($-.10div)/8) - 1
        DQ DROP
        ; stack now contains the digits
        DQ BUF
.pop:   DQ SWAP         ; buf d
        DQ DUP          ; buf d d
        DQ LIT
        DQ 10           ; buf d d 10
        DQ LT           ; buf d flag
        DQ ZEROBRANCH   ; buf d
        DQ (.write-$)/8 - 1
        DQ LIT
        DQ 48           ; buf d 48
        DQ ADD          ; buf ch
        DQ OVER         ; buf ch buf
        DQ CSTORE       ; buf
        DQ LIT
        DQ 1            ; buf 1
        DQ ADD          ; buf+1
        DQ BRANCH
        DQ -(($-.pop)/8) - 1
.write: DQ DROP         ; buf
        DQ LIT
        DQ 10           ; buf 10
        DQ OVER         ; buf 10 buf
        DQ CSTORE       ; buf
        DQ LIT
        DQ 1            ; buf 1
        DQ ADD          ; buf+1
        DQ restofDOT
        DQ NEXTWORD


SECTION .text
GLOBAL _start
_start:
        ; Initialise the model registers.
        mov rbp, stack
        mov r12, continuationstack
        ; Initialising RDX and R9, so as to fake
        ; executing the Forth word IPL.
        mov rdx, program
        mov rbx, ipl+16

stdexe:
        mov [r12], rbx
        add r12, 8
        lea rbx, [rdx+8]
next:
        mov rdx, [rbx]
        add rbx, 8
        mov rax, [rdx]
        jmp rax

;;; Machine code implementations of various Forth words.

NEXTWORD:       DQ $+8
        sub r12, 8
        mov rbx, [r12]
        jmp next

EXECUTE:        DQ $+8
        ; execute the Forth word at TOS
        sub rbp, 8
        mov rdx, [rbp]
        mov rax, [rdx]
        jmp rax

LIT:    DQ $+8
        mov rax, [rbx]
        add rbx, 8
        mov [rbp], rax
        add rbp, 8
        jmp next

ZEROBRANCH:     DQ $+8
        ; read the next word as a relative offset;
        ; pop the TOS and test it;
        ; if it is 0 then branch by adding the offset
        ; to CODEPOINTER.
        mov rcx, [rbx]
        add rbx, 8
        sub rbp, 8
        mov rax, [rbp]
        test rax, rax
        jnz next
        lea rbx, [rbx + 8*rcx]
        jmp next

BRANCH: DQ $+8
        ; read the next word as a relative offset;
        ; branch by adding offset to CODEPOINTER.
        mov rcx, [rbx]
        add rbx, 8
        lea rbx, [rbx + 8*rcx]
        jmp next

SWAP:   DQ $+8
        ; SWAP (A B -- B A)
        mov rax, [rbp-16]
        mov rdx, [rbp-8]
        mov [rbp-16], rdx
        mov [rbp-8], rax
        jmp next

DUP:    DQ $+8
        ; DUP (A -- A A)
        mov rdx, [rbp-8]
        mov [rbp], rdx
        add rbp, 8
        jmp next

OVER:   DQ $+8
        ; OVER (A B -- A B A)
        mov rax, [rbp-16]
        mov [rbp], rax
        add rbp, 8
        jmp next

DROP:   DQ $+8
        ; DROP (A -- )
        sub rbp, 8
        jmp next

EQ0:    DQ $+8
        ; EQ0 (A -- Bool)
        ; Result is -1 (TRUE) if A = 0;
        ; Result is 0 (FALSE) otherwise.
        mov rax, [rbp-8]
        sub rax, 1      ; is-zero now in Carry flag
        sbb rax, rax    ; C=0 -> 0; C=1 -> -1
        mov [rbp-8], rax
        jmp next

LT:     DQ $+8
        ; < (A B -- flag)
        ; flag is -1 (TRUE) if A < B;
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        sub rbp, 16
        cmp rax, rcx    ; C iff B > A
        sbb rax, rax    ; -1 iff B > A
        mov [rbp], rax
        add rbp, 8
        jmp next

ADD:    DQ $+8
        ; + (A B -- sum)
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        add rax, rcx
        sub rbp, 8
        mov [rbp-8], rax
        jmp next

CSTORE: DQ $+8
        ; C! (ch buf -- )
        mov rax, [rbp-16]
        mov rdx, [rbp-8]
        sub rbp, 16
        mov [rdx], al
        jmp next

rdbyte:
        ; read a byte from the lexing buffer
        ; using the pointers lexptr and lexend.
        ; Result is returned in RAX.
        ; If there is a byte, it is returned in the
        ; lower 8 bits of RAX;
        ; otherwise, End Of File condition, -1 is returned.
        mov rdi, [lexptr]
        mov rcx, [lexend]
        sub rcx, rdi
        jnz .nofill
        call fill
.nofill mov rdi, [lexptr]
        mov rcx, [lexend]
        sub rcx, rdi
        jnz .ch
        mov rax, -1
        ret
.ch     mov rax, 0
        mov al, [rdi]
        inc rdi
        mov [lexptr], rdi
        ret
fill:
        ; fill the lexing buffer by
        ; reading some bytes from stdin.
        ; Use a count equal to the size of the buffer
        mov rdi, 0      ; sys_read
        mov rsi, lexbuf
        mov [lexptr], rsi
        mov [lexend], rsi
        mov rdx, lexbufend - lexbuf
        mov rax, 0
        syscall
        test rax, rax
        jle .ret
        mov rdi, lexbuf
        add rdi, rax
        mov [lexend], rdi
.ret:   ret

LEX1:   ; lexes a single word,
        ; using the space available in wordbuf.
        ; ( -- pointer length )
        DQ $+8
        mov r13, wordbuf
.skip:  call rdbyte
        test rax, rax
        js .end
        cmp rax, 32
        jc .skip
.l:     mov [r13], al
        inc r13
        call rdbyte
        test rax, rax
        js .end
        cmp rax, 32
        ja .l
.end:   ; push pointer and length
        sub r13, wordbuf
        mov qword [rbp], wordbuf
        add rbp, 8
        mov [rbp], r13
        add rbp, 8
        jmp next

FIND:   DQ $+8
        ; locate string in dictionary
        ; ( pointer length -- word ) when found
        ; ( pointer length -- pointer length NOTINDICT ) when not found
        mov rax, [DICT]
        ; rax locates the link pointer
        ; (that points to the next word in the dictionary).
.loop:  mov rax, [rax]
        test rax, rax
        jz .empty
        mov r13, [rbp-8]
        mov rcx, [rbp-16]
        ; target string in (rcx, r13)
        mov rdx, [rax]
        mov r14, [rax+8]
        sub rax, 8      ; address of link pointer
        ; dict string in (rdx, r14)
        cmp r13, r14
        jnz .loop       ; lengths don't match, try next
.ch:    test r13, r13
        jz .matched
        mov r8, 0
        mov r9, 0
        mov r8b, [rcx]
        mov r9b, [rdx]
        cmp r8, r9
        jnz .loop
        inc rcx
        inc rdx
        dec r13
        jmp .ch
.matched:
        mov rax, [rax+24]
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
.empty:
        mov rax, NOTINDICT
        mov [rbp], rax
        add rbp, 8
        jmp next

NOTINDICT:      DQ $+8
        ; ( pointer length -- N )
        ; convert to number N.
        sub rbp, 8
        mov rdi, [rbp]
        sub rbp, 8
        mov rsi, [rbp]
        mov rax, 0
.dig:   mov rcx, 10
        mul rcx
        mov rcx, 0
        mov cl, [rsi]
        sub rcx, 48
        jc .end
        add rax, rcx
        inc rsi
        dec rdi
        jnz .dig
.end:   mov [rbp], rax
        add rbp, 8
        jmp next

EXIT:   DQ $+8
        mov rdi, 0
        mov rax, 60
        syscall

DIVMOD: DQ $+8
        ; /MOD (dividend divisor -- quotient remainder)
        ; > r15
        sub rbp, 8
        mov r15, [rbp]
        ; > RAX
        sub rbp, 8
        mov rax, [rbp]

        mov rdx, 0
        idiv r15

        ; RAX >
        mov [rbp], rax
        add rbp, 8
        ; RDX >
        mov [rbp], rdx
        add rbp, 8
        jmp next

restofDOT:      DQ $+8
        ; ( PTR -- )
        ; write contents of buffer to stdout
        mov rdx, [rbp-8]
        sub rdx, buf    ; the buffer length
        mov rdi, 1      ; stdout
        mov rsi, buf
        mov rax, sys_write
        syscall
        jmp next

BUF:    DQ $+8
        mov rdx, buf
        mov [rbp], rdx
        add rbp, 8
        jmp next

QPROMPT: DQ $+8
        ; If interactive and the input buffer is empty,
        ; issue a prompt.
        ; Currently, always assumed interactive.
        mov rdi, [lexptr]
        mov rcx, [lexend]
        cmp rcx, rdi
        jnz next
        ; do syscall
        mov rdi, 1
        mov rsi, prompt
        mov rdx, promptlen
        mov rax, sys_write
        syscall
        jmp next
