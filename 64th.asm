BITS 64

sys_read EQU 0
sys_write EQU 1


SECTION .bss

buf     RESB 8192
buflen  EQU $-buf

lexbuf  RESB 8192       ; buffer for lexing
                        ; (see lexptr and lexend)
lexbufend       EQU $

wordbuf RESB 8192

stack   RESB 100000
continuationstack       RESB 100000


SECTION .data

lexptr DQ lexbuf        ; pointer to next valid byte in lexbuf
lexend DQ lexbuf        ; pointer to limit of valid bytes in lexbuf

prompt DB '> '
promptlen EQU $-prompt

; Start of Dictionary
; The Dictionary is a key Forth datastructure.
; It is a linked list, with each element having the structure:
; - Link Field          1 QWord
; - Name Field: Length  1 QWord
; - Name Field: String  N Bytes
; - Code Field          1 QWord
; - Parameter Field     N Qwords
;
; The Link Field holds the address of the Name Field.
; Thus to read the next Link Field, 8 must be subtracted from this.
; Generally the Link Field points to a numerically lower address.
; A Link Field with contents of 0 marks the end of the dictionary.
;
; This (slightly unusual) organisation means that
; the following dictionary definitions in the assembler file
; are modular, in the sense that
; they can be moved and reordered with editor block operations.

STARTOFDICT:
        DQ 0    ; Link Field

ddup:
        DQ 3
        DB 'dup'
DUP:    DQ $+8          ; std1983
        ; DUP (A -- A A)
        mov rax, [rbp-8]
duprax: mov [rbp], rax
        add rbp, 8
        jmp next
        DQ ddup

dudot:
        DQ 2    ; Name length
        DB 'u.' ; Name
UDOT:                   ; std1983
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
        DQ PLUS         ; buf ch
        DQ OVER         ; buf ch buf
        DQ CSTORE       ; buf
        DQ LIT
        DQ 1            ; buf 1
        DQ PLUS         ; buf+1
        DQ BRANCH
        DQ -(($-.pop)/8) - 1
.write: DQ DROP         ; buf
        DQ LIT
        DQ ' '          ; buf _
        DQ OVER         ; buf _ buf
        DQ CSTORE       ; buf
        DQ LIT
        DQ 1            ; buf 1
        DQ PLUS         ; buf+1
        DQ restofDOT
        DQ NEXTWORD
        DQ dudot         ; Link Field

ddot:
        DQ 1
        DB '.'

DOT:    DQ stdexe
        DQ UDOT
        DQ NEXTWORD
        DQ ddot

dnegate:
        DQ 6
        DB 'negate'

NEGATE:                 ; std1983
        DQ $+8
        mov rax, [rbp-8]
        neg rax
        mov [rbp-8], rax
        jmp next
        DQ dnegate

dplus:
        DQ 1
        DB '+'

PLUS:   DQ $+8          ; std1983
        ; + (A B -- sum)
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        add rax, rcx
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        DQ dplus

dcp:
        DQ 2
        DB 'cp'
CP:     DQ stdvar       ; https://www.forth.com/starting-forth/9-forth-execution/
        DQ dictfree
        DQ dcp

dhere:
        DQ 4
        DB 'here'
HERE:   DQ stdexe       ; std1983
        DQ CP
        DQ FETCH
        DQ NEXTWORD
        DQ dhere

dstore:
        DQ 1
        DB '!'

STORE:  DQ $+8          ; std1983
store0: ; ! ( w addr -- )
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        sub rbp, 16
        mov [rcx], rax
        jmp next
        DQ dstore

dfetch:
        DQ 1
        DB '@'
FETCH:  DQ $+8          ; std1983
        ; @ (addr -- w)
        mov rax, [rbp-8]
        mov rax, [rax]
        mov [rbp-8], rax
        jmp next
        DQ dfetch

dplusstore:
        DQ 2
        DB '+!'
PLUSSTORE:
        DQ stdexe       ; std1983
        ; (w addr -- )
        DQ SWAP         ; (a w)
        DQ OVER         ; (a w a)
        DQ FETCH        ; (a w b)
        DQ PLUS         ; (a s)
        DQ SWAP         ; (s a)
        DQ STORE
        DQ NEXTWORD
        DQ dplusstore

dallot:
        DQ 5
        DB 'allot'

ALLOT:  DQ stdexe       ; std1983
        ; allot (w -- )
        DQ CP
        DQ PLUSSTORE
        DQ NEXTWORD
        DQ dallot

dcomma:
        DQ 1
        DB ','
COMMA:  DQ stdexe       ; std1983
        ; , (w -- )
        DQ HERE
        DQ LIT
        DQ 8
        DQ ALLOT
        DQ STORE
        DQ NEXTWORD
        DQ dcomma

dcmove:
        DQ 5
        DB 'cmove'
CMOVE:  DQ $+8;         ; std1983
cmove0: mov rcx, [rbp-8]
        mov rdi, [rbp-16]
        mov rsi, [rbp-24]
        sub rbp, 24
        mov rdx, 0
.l:     cmp rcx, rdx
        jz next
        mov al, [rsi+rdx]
        mov [rdi+rdx], al
        inc rdx
        jmp .l
        DQ dcmove

dcreate:
        DQ 6
        DB 'create'
CREATE: DQ stdexe       ; std1983
        ; Compile Link Field
        DQ LIT
        DQ DICT
        DQ FETCH
        DQ COMMA
        ; Name Field Address, used much later
        DQ HERE         ; (nfa)
        ; Get Word
        DQ LIT
        DQ ' '
        DQ fWORD        ; (nfa addr)

        ; Compile Name Field
        DQ DUP          ; (nfa addr addr)
        DQ FETCH        ; (nfa addr N)
        DQ LIT
        DQ 8
        DQ PLUS         ; (nfa addr N+8)
        DQ SWAP         ; (nfa N+8 addr)
        DQ OVER         ; (nfa N+8 addr N+8)
        DQ HERE         ; (nfa N+8 addr N+8 here)
        DQ SWAP         ; (nfa N+8 addr here N+8)
        DQ CMOVE        ; (nfa N+8)
        DQ CP           ; (nfa N+8 cp)
        DQ PLUSSTORE    ; (nfa)

        ; Compile Code Field
        DQ LIT
        DQ stdvar
        DQ COMMA
        ; Update Dictionary pointer
        DQ LIT
        DQ DICT         ; (nfa &dict)
        DQ STORE
        DQ NEXTWORD
        DQ dcreate

dtick:
        DQ 1
        DB "'"
TICK:   DQ stdexe       ; std1983
        DQ LIT
        DQ ' '
        DQ fWORD        ; (addr)
        DQ FIND         ; (addr n)
        DQ ZEROBRANCH
        DQ ((.z-$)/8)-1
        DQ NEXTWORD
.z:     DQ DROP
        DQ LIT
        DQ 0
        DQ NEXTWORD
        DQ dtick

dlexlen:
        DQ 6
        DB 'lexlen'
LEXLEN: DQ $+8
        mov rdi, [lexptr]
        mov rcx, [lexend]
        sub rcx, rdi
        mov [rbp], rcx
        add rbp, 8
        jmp next
        DQ dlexlen

dictfree TIMES 8000 DQ 0

DICT:   DQ dlexlen

; (outer) Interpreter loop:
; Fill input bufffer (if cannot, exit);
; Repeat, until the input buffer is empty:
;   WORD: lex single word from input: creates a string.
;   FIND: To convert from string to DICT entry.
;   qEXECUTE: execute / convert number / compile.

INTERACTOR:
        DQ stdexe
        DQ LIT
        DQ 'junk'
.line:  DQ DROP
        DQ QPROMPT
        DQ filbuf
        DQ LEXLEN
        DQ ZEROBRANCH
        DQ ((.x-$)/8)-1
.w:
        DQ LIT
        DQ ' '
        DQ fWORD        ; (addr)
        DQ DUP
        DQ FETCH
        DQ ZEROBRANCH
        DQ -(($-.line)/8)-1
        DQ FIND
        DQ qEXECUTE
        DQ BRANCH
        DQ -(($-.w)/8)-1
.x:     DQ NEXTWORD

ipl:    DQ stdexe
        DQ INTERACTOR
        DQ EXIT

qEXECUTE:
        ; (addr flag -- ...)
        ; addr and flag are typically left by FIND.
        ; if flag is non zero then EXECUTE addr;
        ; otherwise try and handle number.
        DQ stdexe
        DQ ZEROBRANCH
        DQ ((.n-$)/8)-1
        DQ EXECUTE
        DQ NEXTWORD
.n:
        DQ COUNT
        DQ NOTINDICT
        DQ NEXTWORD


SECTION .text
GLOBAL _start
_start:
        ; Initialise the model registers.
        mov rbp, stack
        mov r12, continuationstack
        ; Initialising RDX (aka THIS) and RBX (aka CODEPOINTER),
        ; so as to fake executing the Forth word IPL.
        mov rdx, INTERACTOR
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

stdvar:
        add rdx, 8
        mov [rbp], rdx
        add rbp, 8
        jmp next

;;; Machine code implementations of various Forth words.

NEXTWORD:       DQ $+8
        sub r12, 8
        mov rbx, [r12]
        jmp next

EXECUTE:
        DQ $+8          ; std1983
        ; addr --
        ; execute the Forth word that has compilation address `addr`
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

QDUP:   DQ $+8
        ; ?DUP (NZ -- NZ NZ)    when not zero
        ;      (0 -- 0)         when zero
        mov rax, [rbp-8]
        test rax, rax
        jz next
        jmp duprax

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
        ; If there is a byte, it is returned in RAX
        ; (the byte is 0-extended to fill RAX);
        ; otherwise, End Of File condition, -1 is returned.
        mov rdi, [lexptr]
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

filbuf:
        DQ $+8
        ; fill the lexing buffer by
        ; reading some bytes from stdin.
        ; Use a count equal to the size of the buffer
        mov rdi, 0      ; sys_read
        mov rsi, lexbuf
        mov [lexptr], rsi       ; reset pointers prior to syscall
        mov [lexend], rsi
        mov rdx, lexbufend - lexbuf
        mov rax, 0
        syscall
        test rax, rax
        jle .x
        mov rdi, lexbuf
        add rdi, rax
        mov [lexend], rdi
.x:     jmp next

COUNT:  DQ stdexe       ; std1983
        ; (addr -- addr+8 +n)
        DQ DUP          ; (addr addr)
        DQ LIT
        DQ 8
        DQ PLUS         ; (addr adddr+8)
        DQ SWAP         ; (addr+8 addr)
        DQ FETCH        ; (addr+8 length)
        DQ NEXTWORD

; Note: Can't be called "WORD" as that's a NASM keyword.
fWORD:  ; Doesn't implement Forth standard (yet)
        DQ $+8
fword0:
        sub rbp, 8
        mov r13, wordbuf+8
.skip:  call rdbyte
        test rax, rax   ; RAX < 0 ?
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
.end:   ; Compute length.
        sub r13, wordbuf+8
        ; Store length to make a counted string.
        mov [wordbuf], r13
        ; Push address of counted string.
        mov qword [rbp], wordbuf
        add rbp, 8
        jmp next

FIND:   DQ $+8          ; std1983
        ; search and locate string in dictionary
        ; ( addr1 -- addr2 trueish ) when found
        ; ( addr1 -- addr1 ff ) when not found
        mov rsi, [rbp-8]        ; RSI is addr of counted string.
        mov rax, DICT+8
        ; rax holds address of (length, pointer) name pair.
        ; Immediately before that is the Link Field
        ; (that points to the next word in the dictionary).
.loop:  mov rax, [rax-8]
        test rax, rax
        jz .empty
        mov r13, [rsi]          ; length
        lea rcx, [rsi+8]        ; pointer
        ; target string in (rcx, r13)
        mov r14, [rax]          ; length of dict name
        lea rdx, [rax+8]        ; pointer to dict name
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
        jnz .loop       ; byte doesn't match, try next
        inc rcx
        inc rdx
        dec r13
        jmp .ch
.matched:
        ; Skip over length and name bytes,
        ; storing Code Field Address in RAX (and then replace TOS).
        lea rax, [rax + 8 + r14]
        mov [rbp-8], rax
        ; Push true (-1) for non-immediate word.
        mov qword [rbp], -1
        add rbp, 8
        jmp next
.empty:
        mov qword [rbp], 0
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
        sub rbp, 8
        mov rdx, [rbp]
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
        mov rdi, 2      ; stderr
        mov rsi, prompt
        mov rdx, promptlen
        mov rax, sys_write
        syscall
        jmp next
