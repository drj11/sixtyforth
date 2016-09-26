BITS 64

sys_read EQU 0
sys_write EQU 1


SECTION .bss

picture RESB 300        ; For picture output, <# and so on.
tibaddr RESB 500        ; (address of) Terminal Input Buffer
                        ; (see >IN and #TIB for pointer and size)
tibend  EQU $
tibsize EQU tibend - tibaddr

wordbuf RESB 8192

stack   RESB 100000
returnstack       RESB 100000


SECTION .data

prompt DB '> '
promptlen EQU $-prompt


; Start of Dictionary
; The Dictionary is a key Forth datastructure.
; It is a linked list, with each element having the structure:
; - Link Field          1 QWord
; - Name Field: Length  1 QWord
; - Name Field: String  8 Bytes
; - Code Field          1 QWord
; - Parameter Field     N Qwords
;
; The Link Field holds the address of the previous Link Field.
; Thus to locate the previous Name Field, 8 must be added to this.
; Generally the Link Field points to a numerically lower address.
; A Link Field with contents of 0 marks the end of the dictionary.
;
; This (slightly unusual) organisation means that
; the following dictionary definitions in the assembler file
; are modular, in the sense that
; they can be moved and reordered with editor block operations.
;
; (an idea borrowed from [LOELIGER1981]) Note that in the
; Name Field only the first 8 bytes of a name are stored,
; even though the length of the whole name is stored.
; This means that the Name Field is fixed size,
; but can still distinguish between
; names of different lengths that share a prefix.
;
; The Length field also holds flags. It is a 64-bit word
; that holds the length in the least significant 32-bits,
; and flags in the upper 32 bits.
; The only flag that is used currently is bit 33 (2<<32),
; which is 1 when the word is marked as IMMEDIATE.

; For creating link pointers in the dictionary.
%define Link(a) DQ (a)-8
; Or (using «|») into Length Field to create IMMEDIATE word.
%define Immediate (2<<32)

STARTOFDICT:
        DQ 0    ; Link Field

ddup:
        DQ 3
        DQ 'dup'        ; std1983
DUP:    DQ $+8
        ; DUP (A -- A A)
        mov rax, [rbp-8]
duprax: mov [rbp], rax
        add rbp, 8
        jmp next
        Link(ddup)

drot:
        DQ 3
        DQ 'rot'        ; std1983
ROT:    DQ $+8
        mov rax, [rbp-8]
        mov rcx, [rbp-16]
        mov rdx, [rbp-24]
        mov [rbp-24], rcx
        mov [rbp-16], rax
        mov [rbp-8], rdx
        jmp next
        Link(drot)

ddepth:
        DQ 5
        DQ 'depth'      ; std1983
DEPTH:  DQ $+8
        ; DEPTH ( -- +n)
        mov rcx, stack
        mov rax, rbp
        sub rax, rcx
        shr rax, 3
        jmp duprax
        Link(ddepth)

dtonumber:
        DQ 7
        DQ '>number'    ; std1994
toNUMBER:
        DQ $+8
        ; ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        ; :todo: only works in single range
        mov rdi, [rbp-8]        ; length remaining
        mov rsi, [rbp-16]       ; pointer
        mov rax, [rbp-32]       ; (partial) converted number
        mov r8, [abase]         ; BASE
.dig:
        mov rcx, 0
        mov cl, [rsi]
        sub rcx, 48     ; Convert from ASCII digit
        jc .end         ; ASCII value < '0'
        cmp rcx, 10
        jc .digitok     ; ASCII value <= '9'
        sub rcx, 7      ; number of chars between '9' and 'A' in ASCII
        jc .end         ; '9' < ASCII value < 'A'
        cmp rcx, r8
        jnc .end        ; BASE <= numeric value
.digitok:
        mul r8
        add rax, rcx
        inc rsi
        dec rdi
        jnz .dig
.end:   mov [rbp-32], rax
        mov [rbp-24], rdx
        mov [rbp-16], rsi
        mov [rbp-8], rdi
        jmp next
        Link(dtonumber)

dudot:
        DQ 2
        DQ 'u.'         ; std1983
Udot:   DQ stdexe
        DQ LIT
        DQ 0
        DQ lesssharp
        DQ fBL
        DQ HOLD
        DQ sharpS
        DQ sharpgreater
        DQ TYPE
        DQ EXIT
        Link(dudot)

ddot:
        DQ 1
        DQ '.'          ; std1983
dot:    DQ stdexe
        DQ DUP          ; (n n)
        DQ fABS         ; (n +n)
        DQ LIT
        DQ 0            ; (n +n 0)
        DQ lesssharp
        DQ fBL
        DQ HOLD
        DQ sharpS
        DQ ROT
        DQ SIGN
        DQ sharpgreater
        DQ TYPE
        DQ EXIT
        Link(ddot)

dbase:
        DQ 4
        DQ 'base'       ; std1983
BASE:   DQ stdvar
abase:  DQ 10
        Link(dbase)

dpic:
        DQ 3
        DQ 'pic'
PIC:    DQ stdvar
        DQ 0
        Link(dpic)

dlesssharp:
        DQ 2
        DQ '<#'         ; std1983
lesssharp:
        DQ stdexe
        DQ LIT
        DQ tibaddr
        DQ PIC
        DQ store
        DQ EXIT
        Link(dlesssharp)

dsharp:
        DQ 1
        DQ '#'          ; std1983
sharp:  DQ stdexe
        ; # (+d1 -- +d2)
        ; :todo: Only works for single range numbers
        DQ BASE
        DQ fetch        ; (d b)
        DQ UMslashMOD   ; (q r)
        DQ DIGIT        ; (q ascii)
        DQ HOLD         ; (q)
        DQ LIT
        DQ 0            ; (q 0)
        DQ EXIT
        Link(dsharp)

dhold:
        DQ 4
        DQ 'hold'       ; std1983
HOLD:   DQ stdexe
        DQ PIC
        DQ fetch        ; (ascii pic)
        DQ oneminus     ; (ascii addr)
        DQ SWAP         ; (addr ascii)
        DQ OVER         ; (addr ascii addr)
        DQ Cstore       ; (addr)
        DQ PIC
        DQ store
        DQ EXIT
        Link(dhold)

dsharpgreater:
        DQ 2
        DQ '#>'         ; std1983
sharpgreater:
        DQ stdexe
        ; #> (d+ -- addr +n)
        DQ DROP
        DQ DROP
        DQ PIC
        DQ fetch        ; (addr)
        DQ LIT
        DQ tibaddr      ; (addr tib)
        DQ OVER         ; (addr tib addr)
        DQ MINUS        ; (addr +n)
        DQ EXIT
        Link(dsharpgreater)

dsharps:
        DQ 2
        DQ '#s'         ; std1983
sharpS:
        DQ stdexe
.l:     DQ sharp        ; (d+)
        DQ OVER
        DQ OVER         ; (d+ d+)
        DQ OR           ; (d+ n)
        DQ zequals
        DQ ZEROBRANCH
        DQ -(($-.l)/8)-1
        DQ EXIT
        Link(dsharps)

dsign:
        DQ 4
        DQ 'sign'       ; std1983
SIGN:   DQ stdexe
        DQ zless
        DQ ZEROBRANCH
        DQ ((.pos-$)/8)-1
        DQ LIT
        DQ '-'
        DQ HOLD
.pos:   DQ EXIT
        Link(dsign)

ddigit:
        DQ 5
        DQ 'digit'
DIGIT:  DQ stdexe
        ; DIGIT (n -- ascii)
        ; convert digit (0 to 15) to ASCII
        ; 0 -> 48
        ; 10 -> 65
        DQ LIT
        DQ 9            ; (n 9)
        DQ OVER         ; (n 9 n)
        DQ lessthan     ; (n bf)
        DQ ZEROBRANCH
        DQ ((.l-$)/8)-1
        DQ LIT
        DQ 7
        DQ PLUS
.l:     DQ LIT
        DQ '0'
        DQ PLUS
        DQ EXIT
        Link(ddigit)

dbl:
        DQ 2
        DQ 'bl'         ; std1994
fBL:    DQ stdexe
        DQ LIT
        DQ ' '
        DQ EXIT
        Link(dbl)

dtype:
        DQ 4
        DQ 'type'       ; std1983
TYPE:   DQ $+8
        ; TYPE (addr +n -- )
        mov rdx, [rbp-8]
        mov rsi, [rbp-16]
        sub rbp, 16
        mov rdi, 1      ; stdout
        mov rax, sys_write
        syscall
        jmp next
        Link(dtype)

dcount:
        DQ 5
        DQ 'count'      ; std1983 - modified
COUNT:  DQ stdexe
        ; (addr -- addr+8 +n)
        DQ DUP          ; (addr addr)
        DQ LIT
        DQ 8
        DQ PLUS         ; (addr adddr+8)
        DQ SWAP         ; (addr+8 addr)
        DQ fetch        ; (addr+8 length)
        DQ EXIT
        Link(dcount)

dzequals:
        DQ 2
        DQ '0='         ; std1983
zequals:
        DQ $+8
        ; 0= (A -- Bool)
        ; Result is -1 (TRUE) if A = 0;
        ; Result is 0 (FALSE) otherwise.
        mov rax, [rbp-8]
        sub rax, 1      ; is-zero now in Carry flag
        sbb rax, rax    ; C=0 -> 0; C=1 -> -1
        mov [rbp-8], rax
        jmp next
        Link(dzequals)

dzless:
        DQ 2
        DQ '0<'         ; std1983
zless:  DQ $+8
        ; 0< (n -- true) when n < 0
        ;    (n -- false) otherwise
        mov rax, [rbp-8]
        mov rcx, -1
        test rax, rax
        js .sk
        add rcx, 1
.sk:    mov [rbp-8], rcx
        jmp next
        Link(dzless)

dlessthan:
        DQ 1
        DQ '<'          ; std1983
lessthan:
        DQ $+8
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        cmp rax, rcx
        sbb rax, rax
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        Link(dlessthan)

dnegate:
        DQ 6
        DQ 'negate'     ; std1983
NEGATE:
        DQ $+8
        mov rax, [rbp-8]
        neg rax
        mov [rbp-8], rax
        jmp next
        Link(dnegate)

dabs:
        DQ 3
        DQ 'abs'        ; std1983
fABS:   DQ $+8
        mov rax, [rbp-8]
        test rax, rax
        jns .pos
        neg rax
.pos:   mov [rbp-8], rax
        jmp next
        Link(dabs)

dplus:
        DQ 1
        DQ '+'          ; std1983
PLUS:   DQ $+8
        ; + (A B -- sum)
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        add rax, rcx
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        Link(dplus)

dminus:
        DQ 1
        DQ '-'          ; std1983
MINUS:  DQ $+8
        ; - ( A B -- difference)
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        sub rax, rcx
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        Link(dminus)

doneminus:
        DQ 2
        DQ '1-'         ; std1983
oneminus:
        DQ $+8
        mov rax, [rbp-8]
        sub rax, 1
        mov [rbp-8], rax
        jmp next
        Link(doneminus)

ddivide:
        DQ 1
        DQ '/'          ; std1983
divide: DQ stdexe
        DQ divMOD
        DQ DROP
        DQ EXIT
        Link(ddivide)

dumslashmod:
        DQ 6
        DQ 'um/mod'     ; std1994
UMslashMOD:
        DQ $+8
        ; UM/MOD (ud u1 -- uq ur)
        ; Note: Double Single -> Single Single
        ; > r15
        sub rbp, 8
        mov r15, [rbp]
        ; > RDX
        sub rbp, 8
        mov rdx, [rbp]
        ; > RAX
        sub rbp, 8
        mov rax, [rbp]

        div r15

        ; RAX >
        mov [rbp], rax
        add rbp, 8
        ; RDX >
        mov [rbp], rdx
        add rbp, 8
        jmp next
        Link(dumslashmod)

dtrue:
        DQ 4
        DQ 'true'       ; std1994
TRUE:   DQ stdexe
        DQ LIT
        DQ -1
        DQ EXIT
        Link(dtrue)

dfalse:
        DQ 5
        DQ 'false'      ; std1994
FALSE:  DQ stdexe
        DQ LIT
        DQ 0
        DQ EXIT
        Link(dfalse)

dor:
        DQ 2
        DQ 'or'         ; std1983
OR:     DQ $+8
        ; OR (A B -- bitwise-or)
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        or rax, rcx
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        Link(dor)

dcp:
        DQ 2
        DQ 'cp'
CP:     DQ stdvar       ; https://www.forth.com/starting-forth/9-forth-execution/
        DQ dictfree
        Link(dcp)

dhere:
        DQ 4
        DQ 'here'       ; std1983
HERE:   DQ stdexe
        DQ CP
        DQ fetch
        DQ EXIT
        Link(dhere)

dnumbertib:
        DQ 4
        DQ '#tib'       ; std1983
numberTIB:
        DQ stdvar
anumberTIB:
        DQ 0
        Link(dnumbertib)

dtoin:
        DQ 3
        DQ '>in'        ; std1983
toIN:   DQ stdvar
atoIN:  DQ 0
        Link(dtoin)

dstore:
        DQ 1
        DQ '!'          ; std1983
store:  DQ $+8
store0: ; ! ( w addr -- )
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        sub rbp, 16
        mov [rcx], rax
        jmp next
        Link(dstore)

dfetch:
        DQ 1
        DQ '@'          ; std1983
fetch:  DQ $+8
        ; @ (addr -- w)
        mov rax, [rbp-8]
        mov rax, [rax]
        mov [rbp-8], rax
        jmp next
        Link(dfetch)

dcfetch:
        DQ 2
        DQ 'c@'         ; std1983
Cfetch: DQ $+8
        ; C@ (addr -- ch)
        mov rdx, [rbp-8]
        xor rax, rax
        mov al, [rdx]
        mov [rbp-8], rax
        jmp next
        Link(dcfetch)

dplusstore:
        DQ 2
        DQ '+!'         ; std1983
plusstore:
        DQ stdexe
        ; (w addr -- )
        DQ SWAP         ; (a w)
        DQ OVER         ; (a w a)
        DQ fetch        ; (a w b)
        DQ PLUS         ; (a s)
        DQ SWAP         ; (s a)
        DQ store
        DQ EXIT
        Link(dplusstore)

dswap:
        DQ 4
        DQ 'swap'       ; std1983
SWAP:   DQ $+8
        ; SWAP (A B -- B A)
        mov rax, [rbp-16]
        mov rdx, [rbp-8]
        mov [rbp-16], rdx
        mov [rbp-8], rax
        jmp next
        Link(dswap)

dqdup:
        DQ 4
        DQ '?dup'       ; std1983
qDUP:   DQ $+8
        ; ?DUP (NZ -- NZ NZ)    when not zero
        ;      (0 -- 0)         when zero
        mov rax, [rbp-8]
        test rax, rax
        jz next
        jmp duprax
        Link(dqdup)

dover:
        DQ 4
        DQ 'over'       ; std1983
OVER:   DQ $+8
        ; OVER (A B -- A B A)
        mov rax, [rbp-16]
        mov [rbp], rax
        add rbp, 8
        jmp next
        Link(dover)

ddrop:
        DQ 4
        DQ 'drop'       ; std1983
DROP:   DQ $+8
        ; DROP (A -- )
        sub rbp, 8
        jmp next
        Link(ddrop)

dallot:
        DQ 5
        DQ 'allot'      ; std1983
ALLOT:  DQ stdexe
        ; allot (w -- )
        DQ CP
        DQ plusstore
        DQ EXIT
        Link(dallot)

dcomma:
        DQ 1
        DQ ','          ; std1983
comma:  DQ stdexe
        ; , (w -- )
        DQ HERE
        DQ LIT
        DQ 8
        DQ ALLOT
        DQ store
        DQ EXIT
        Link(dcomma)

dliteral:
        DQ 7 | Immediate
        DQ 'literal'    ; std1983
LITERAL:
        DQ stdexe
        DQ LIT
        DQ LIT          ; haha, weird or what?
        DQ comma
        DQ comma
        DQ EXIT
        Link(dliteral)

dcmove:
        DQ 5
        DQ 'cmove'      ; std1983
CMOVE:  DQ $+8;
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
        Link(dcmove)

dcreate:
        DQ 6
        DQ 'create'     ; std1983
CREATE: DQ stdexe
        ; Link Field Address, used much later
        DQ HERE         ; (lfa)
        ; Compile Link Field
        DQ LIT
        DQ DICT
        DQ fetch
        DQ comma
        ; Get Word
        DQ fBL
        DQ fWORD        ; (lfa addr)

        ; Compile Name Field
        ; Note: this copies the entire name string
        ; into the dictionary, even though
        ; only 8 bytes of the string are used.
        ; The Code Field will overwrite bytes 9 to 16 of a name
        ; if it is that long.
        ; This works as long as you don't run out of dictionary space.
        ; But is not very tidy.
        DQ DUP          ; (lfa addr addr)
        DQ fetch        ; (lfa addr N)
        DQ LIT
        DQ 8
        DQ PLUS         ; (lfa addr N+8)
        DQ HERE         ; (lfa addr N+8 here)
        DQ SWAP         ; (lfa addr here N+8)
        DQ CMOVE        ; (lfa)
        DQ LIT
        DQ 16
        DQ CP           ; (lfa 16 cp)
        DQ plusstore    ; (lfa)

        ; Compile Code Field
        DQ LIT
        DQ stdvar
        DQ comma
        ; Update Dictionary pointer
        DQ LIT
        DQ DICT         ; (lfa &dict)
        DQ store
        DQ EXIT
        Link(dcreate)

dtick:
        DQ 1
        DQ "'"          ; std1983
TICK:   DQ stdexe
        DQ fBL
        DQ fWORD        ; (addr)
        DQ FIND         ; (addr n)
        DQ ZEROBRANCH
        DQ ((.z-$)/8)-1
        DQ EXIT
.z:     DQ DROP
        DQ LIT
        DQ 0
        DQ EXIT
        Link(dtick)

dtobody:
        DQ 5
        DQ '>body'      ; std1983
toBODY: DQ stdexe
.body:  DQ LIT
        DQ .body - toBODY       ; 8, basically
        DQ PLUS
        DQ EXIT
        Link(dtobody)

dfrombody:
        DQ 5
        DQ 'body>'      ; std1983[harris]
fromBODY:
        DQ stdexe
.body:  DQ LIT
        DQ .body - fromBODY     ; 8, basically
        DQ MINUS
        DQ EXIT
        Link(dfrombody)

dstate:
        DQ 5
        DQ 'state'      ; std1983
STATE:  DQ stdvar
stateaddr:
        DQ 0
        Link(dstate)

dket:
        DQ 1
        DQ ']'          ; std1983
ket:    DQ stdexe
        DQ LIT
        DQ 1
        DQ STATE
        DQ store
        DQ EXIT
        Link(dket)

dcolon:
        DQ 1
        DQ ':'          ; std1983
colon:  DQ stdexe
        DQ CREATE
        DQ LIT
        DQ stdexe
        DQ HERE
        DQ fromBODY
        DQ store
        DQ ket
        DQ EXIT
        Link(dcolon)

dsemicolon:
        DQ 1 | Immediate
        DQ ';'          ; std1983
semicolon:
        DQ stdexe
        DQ LIT
        DQ EXIT
        DQ comma
        ; :todo: check compiler safety
        DQ LIT
        DQ 0
        DQ STATE
        DQ store
        DQ EXIT
        Link(dsemicolon)

dexit:
        DQ 4
        DQ 'exit'       ; std1983
EXIT:   DQ $+8
        sub r12, 8
        mov rbx, [r12]
        jmp next
        Link(dexit)

dtib:
        DQ 3
        DQ 'tib'        ; std1983
TIB:    DQ $+8
        mov qword [rbp], tibaddr
        add rbp, 8
        jmp next
        Link(dtib)

dfword:
        DQ 4
        DQ 'word'
; Note: Can't be called "WORD" as that's a NASM keyword.
fWORD:  DQ $+8
        ; Doesn't implement Forth standard (yet)
        ; Register usage:
        ; RAX character / temp
        ; RDX delimiter
        ; R13 current pointer (into wordbuf)
fword0:
        sub rbp, 8
        mov rdx, [rbp]  ; delimiter in RDX
        mov r13, wordbuf+8
.skip:  call rdbyte
        test rax, rax   ; RAX < 0 ?
        js .end
        ; Skip Control Codes
        cmp rax, 32
        jc .skip
        ; Skip delimiter
        cmp rax, rdx
        jz .skip
.l:     mov [r13], al
        inc r13
        call rdbyte
        test rax, rax
        js .end
        ; Test for Control Codes
        cmp rax, 32
        jb .end
        ; Test for delimiter
        cmp rax, rdx
        jnz .l
.end:   ; Compute length.
        sub r13, wordbuf+8
        ; Store length to make a counted string.
        mov [wordbuf], r13
        ; Push address of counted string.
        mov qword [rbp], wordbuf
        add rbp, 8
        jmp next
        Link(dfword)

dfind:
        DQ 4
        DQ 'find'       ; std1983
FIND:   DQ $+8
        ; search and locate string in dictionary
        ; ( addr1 -- addr2 trueish ) when found
        ; ( addr1 -- addr1 ff ) when not found
        mov rsi, [rbp-8]        ; RSI is addr of counted string.
        mov rax, DICT   ; Link to most recent word
        ; rax points to Link Field
        ; (that points to the next word in the dictionary).
.loop:  mov rax, [rax]
        test rax, rax
        jz .empty
        mov r13, [rsi]          ; length
        lea rcx, [rsi+8]        ; pointer
        ; target string in (rcx, r13)
        mov r14, [rax+8]        ; length of dict name
        ; mask off flags
        mov rdx, 0xffffffff
        and r14, rdx
        lea rdx, [rax+16]       ; pointer to dict name
        ; dict string in (rdx, r14)
        cmp r13, r14
        jnz .loop       ; lengths don't match, try next
        ; The dictionary only holds 8 bytes of name,
        ; so we must check at most 8 bytes.
        cmp r13, 8
        jle .ch         ; <= 8 already
        mov r13, 8      ; clamp to length 8
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
        ; fetch flags
        mov rdx, [rax+8]
        shr rdx, 32
        ; Skip over Link and Name Field (length and 8 name bytes),
        ; storing Code Field Address in RAX (and then replace TOS).
        lea rax, [rax + 24]
        mov [rbp-8], rax
        ; std1983 requires -1 (true) for non-immediate word,
        ; and 1 for immediate word.
        ; Flags (rdx) is 0 for non-immediate; 2 for immediate.
        ; So we can subtract 1.
        sub rdx, 1
        mov [rbp], rdx
        add rbp, 8
        jmp next
.empty:
        mov qword [rbp], 0
        add rbp, 8
        jmp next
        Link(dfind)

dquit:
        DQ 4
        DQ 'quit'       ; std1983
QUIT:   DQ reset
        Link(dquit)

duseless:
        DQ 7
        DQ 'useless'
USELESS:
        DQ stdvar
        Link(duseless)

dictfree TIMES 8000 DQ 0

DICT:   Link(duseless)

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
        DQ filbuf       ; basically QUERY from std
        DQ numberTIB
        DQ fetch
        DQ ZEROBRANCH
        DQ ((.x-$)/8)-1
.w:
        DQ fBL
        DQ fWORD        ; (addr)
        DQ DUP          ; (addr addr)
        DQ COUNT        ; (addr a n)
        DQ SWAP
        DQ DROP         ; (addr n)
        DQ ZEROBRANCH
        DQ -(($-.line)/8)-1
        DQ FIND
        DQ qEXECUTE
        DQ BRANCH
        DQ -(($-.w)/8)-1
.x:     DQ EXIT

ipl:    DQ stdexe
        DQ INTERACTOR
        DQ sysEXIT

qEXECUTE:
        ; (addr flag -- ...)
        ; addr and flag are typically left by FIND.
        ; if flag is non zero then EXECUTE addr;
        ; otherwise try and handle number.
        DQ stdexe
        DQ qDUP
        DQ ZEROBRANCH
        DQ ((.n-$)/8)-1
        ; (addr +-1)
        ; immediate=1; non-immediate=-1
        DQ LIT
        DQ 1
        DQ PLUS         ; (addr 0/2)
        DQ STATE
        DQ fetch        ; (addr 0/2 compiling?)
        DQ zequals      ; (addr 0/2 interpreting?)
        DQ OR           ; (addr 0/2)
        ; 0=compile; 2=execute
        DQ ZEROBRANCH
        DQ ((.comp-$)/8)-1
        DQ EXECUTE
        DQ EXIT
.comp:  ; (addr)
        DQ comma
        DQ EXIT
.n:     ; (addr)
        DQ qNUMBER
        DQ ZEROBRANCH
        DQ ((.abort-$)/8)-1
        ; (n)
        DQ STATE
        DQ fetch
        ; (n compiling?)
        DQ ZEROBRANCH
        DQ ((.x-$)/8)-1
        DQ LITERAL
.x:
        DQ EXIT
.abort:
        DQ COUNT
        DQ TYPE
        DQ LIT
        DQ .error
        DQ LIT
        DQ 2
        DQ TYPE
        DQ QUIT
.error: DQ ' ?'

qNUMBER:
        DQ stdexe
        ; ?NUMBER (c-string -- n true) if convertible
        ;         (c-string -- c-string false) if not convertible
        DQ DUP
        DQ FALSE
        DQ FALSE        ; (c-string c-string 0 0)
        DQ ROT          ; (c-string 0 0 c-string)
        DQ COUNT        ; (c-string 0 0 addr +n)
        DQ toNUMBER     ; (c-string ud a n)
        DQ ZEROBRANCH
        DQ ((.success-$)/8)-1
        ; (c-string ud a)
        DQ DROP
        DQ DROP
        DQ DROP
        DQ FALSE
        DQ EXIT
.success:
        ; (c-string ud a)
        DQ DROP         ; (c-string ud)
        DQ ROT          ; (ud c-string)
        DQ DROP         ; (ud)
        DQ DROP         ; (u)
        DQ TRUE         ; (u true)
        DQ EXIT


SECTION .text
GLOBAL _start
_start:
        ; Initialise the model registers.
        mov rbp, stack
reset:  ; QUIT jumps here
        mov r12, returnstack
        mov rax, stateaddr
        mov qword [rax], 0
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

ZEROBRANCH:
        DQ $+8
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

Cstore: DQ $+8
        ; C! (ch buf -- )
        mov rax, [rbp-16]
        mov rdx, [rbp-8]
        sub rbp, 16
        mov [rdx], al
        jmp next

rdbyte:
        ; read a byte from the TIB
        ; using #TIB and >IN.
        ; Result is returned in RAX.
        ; If there is a byte, it is returned in RAX
        ; (the byte is 0-extended to fill RAX);
        ; otherwise, End Of File condition, -1 is returned.
        mov rdi, [atoIN]
        mov rcx, [anumberTIB]
        sub rcx, rdi
        jnz .ch
        mov rax, -1
        ret
.ch     mov rax, 0
        lea rcx, [rdi +  tibaddr]
        mov al, [rcx]
        inc rdi
        mov [atoIN], rdi
        ret

filbuf:
        DQ $+8
        ; fill the lexing buffer by
        ; reading some bytes from stdin.
        ; Use a count equal to the size of the buffer
        mov rdi, sys_read
        mov rsi, tibaddr
        mov qword [atoIN], 0    ; reset pointers prior to syscall
        mov qword [anumberTIB], 0
        mov rdx, tibsize
        mov rax, 0
        syscall
        test rax, rax
        jle .x          ; :todo: check for errors
        add rdi, rax
        mov [anumberTIB], rax
.x:     jmp next

sysEXIT:
        DQ $+8
        mov rdi, 0
        mov rax, 60
        syscall

divMOD: DQ $+8
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

QPROMPT: DQ $+8
        ; If interactive and the input buffer is empty,
        ; issue a prompt.
        ; Currently, always assumed interactive.
        mov rdi, [atoIN]
        mov rcx, [anumberTIB]
        cmp rcx, rdi
        jnz next
        ; do syscall
        mov rdi, 2      ; stderr
        mov rsi, prompt
        mov rdx, promptlen
        mov rax, sys_write
        syscall
        jmp next
