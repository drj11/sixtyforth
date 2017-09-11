BITS 64


sys_read EQU 0
sys_write EQU 1
sys_mmap EQU 9

extern _binary_rc_4_start
extern _binary_rc_4_size


SECTION .bss

ib0 RESB 500            ; Input Block 0
ib1 RESB 500            ; Input Block 1

ibsize EQU ib1 - ib0

wordbuf RESB 8192

stack   RESB 100000
returnstack       RESB 100000

emitbuf RESB 1


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

; Convert address of Code Field to address of Link Field
%define CtoL(a) DQ (a)-24
; Or (using «|») into Length Field to create IMMEDIATE word.
%define Immediate (2<<32)

; Assembler Style
; For Forth words defined in assembler,
; try and stick to the following guidelines:

; :asm:load-store:
; Pretend Intel 64 is a Load/Store architecture:
; prefer instructions that either just do load/store, or
; just do register to register computation.

; :asm:fetch-do-store:
; Words should be organised to do in order:
; - fetch data from stack to registers;
; - do computation;
; - store data from registers to stack.

; :asm:memseq:
; In general access memory sequentially from lower addresses to
; higher addresses.

STARTOFDICT:
        DQ 0    ; Link Field

        DQ 8
        DQ 'syscall3'
SYSCALL3:
        DQ $+8
        ; SYSCALL3 ( a b c n -- rax )
        ; Call syscall n with 3 arguments;
        ; return with RAX left on stack.
        mov rdi, [rbp-32]
        mov rsi, [rbp-24]
        mov rdx, [rbp-16]
        ; syscall number
        mov rax, [rbp-8]
        sub rbp, 24
        syscall
        mov [rbp-8], rax
        jmp next
        CtoL(SYSCALL3)

        DQ 8
        DQ 'syscall6'
SYSCALL6:
        DQ $+8
        ; SYSCALL6 ( a b c d e f n -- rax )
        ; Call syscall n with 6 argument;
        ; return with RAX left on stack.
        mov rdi, [rbp-56]
        mov rsi, [rbp-48]
        mov rdx, [rbp-40]
        mov r10, [rbp-32]
        mov r8, [rbp-24]
        mov r9, [rbp-16]
        mov rax, [rbp-8]
        sub rbp, 48
        syscall
        mov [rbp-8], rax
        jmp next
        CtoL(SYSCALL6)

        DQ 7
        DQ 'sysread'
SYSREAD:
        DQ stdexe
        DQ LIT, sys_read
        DQ SYSCALL3
        DQ EXIT
        CtoL(SYSREAD)

        DQ 7
        DQ 'sysexit'
sysEXIT:
        DQ $+8
        mov rdi, 0
        mov rax, 60
        syscall
        CtoL(sysEXIT)

        DQ 7
        DQ 'execute'
EXECUTE:
        DQ $+8          ; std1983
        ; EXECUTE ( addr -- )
        ; execute the Forth word that has compilation address `addr`
        sub rbp, 8
        mov rdx, [rbp]
        mov rax, [rdx]
        jmp rax
        CtoL(EXECUTE)

        DQ 3
        DQ 'rsp'
fRSP:
        DQ $+8
        ; Push the RSP register onto the Forth stack.
        mov rax, rsp
        jmp pushrax
        CtoL(fRSP)

        DQ 3
        DQ 'dup'        ; std1983
DUP:    DQ $+8
        ; DUP ( a -- a a )
        mov rax, [rbp-8]
pushrax:
        mov [rbp], rax
        add rbp, 8
        jmp next
        CtoL(DUP)

        DQ 3
        DQ 'rot'        ; std1983
ROT:    DQ $+8
        mov rdx, [rbp-24]
        mov rcx, [rbp-16]
        mov rax, [rbp-8]
        mov [rbp-24], rcx
        mov [rbp-16], rax
        mov [rbp-8], rdx
        jmp next
        CtoL(ROT)

        DQ 5
        DQ 'depth'      ; std1983
DEPTH:  DQ $+8
        ; DEPTH ( -- +n )
        mov rcx, stack
        mov rax, rbp
        sub rax, rcx
        shr rax, 3
        jmp pushrax
        CtoL(DEPTH)

        DQ 6
        DQ 'within'     ; std1994
WITHIN:
        DQ stdexe
        ; WITHIN ( test low high -- flag )
        ; Implementation as per [FORTH1994]
        DQ OVER
        DQ MINUS
        DQ toR
        DQ MINUS
        DQ Rfrom
        DQ Ulessthan
        DQ EXIT
        CtoL(WITHIN)

        DQ 7
        DQ '>number'    ; std1994
toNUMBER:
        DQ stdexe
        ; ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
.begin:
        DQ DUP          ; ud c-addr u u
        DQ ZEROBRANCH
        DQ .x - $
        DQ ASCIItoDIGIT ; ud c-addr u n
        DQ DUP, zless   ; ud c-addr u n bf
        DQ ZEROBRANCH
        DQ .ok - $
        DQ DROP
.x:
        DQ EXIT
.ok:
        ; ud c-addr u n
        DQ toR          ; ud c-addr u
        DQ twoSWAP      ; c-addr u ud
        DQ BASE, fetch  ; c-addr u ud base
        DQ UDstar       ; c-addr u ud
        DQ Rfrom        ; c-addr u ud n
        DQ z            ; c-addr u ud n 0
        DQ Dplus        ; c-addr u ud
        DQ twoSWAP      ; ud c-addr
        DQ BRANCH
        DQ -($ - .begin)
ASCIItoDIGIT:
        DQ stdexe
        ; factor of >NUMBER
        ; ( c-addr u -- c-addr u n )
        ; c-addr and u are updated
        ; n is the next digit;
        ; n is negative if there is no next digit.
        DQ OVER, Cfetch ; c-addr u ch
        DQ DUP          ; c-addr u ch ch
        DQ LIT, '0'     ; c-addr u ch ch '0'
        DQ LIT, '9'+1   ; c-addr u ch ch '0' ':'
        DQ WITHIN       ; c-addr u ch bf
        ; ok if '0' <= ch <= '9'
        DQ ZEROBRANCH
        DQ .then - $
        DQ LIT, '0'
        DQ MINUS        ; c-addr u c
        DQ digitadvance
        DQ EXIT
.then:
        DQ LIT, 'A'-10  ; c-addr u c 'A'
        DQ MINUS        ; c-addr u c
        ; Now, A -> 10, B -> 11, and so on.
        DQ DUP          ; c-addr u c c
        DQ LIT, 10      ; c-addr u c c 10
        DQ BASE, fetch  ; c-addr u c c 10 base
        DQ WITHIN       ; c-addr u c bf
        DQ ZEROBRANCH
        DQ .then2 - $
        DQ digitadvance
        DQ EXIT
.then2:
        DQ DROP
        DQ TRUE
        DQ EXIT
digitadvance:
        DQ stdexe
        ; Factor of ASCIItoDIGIT
        ; ( c-addr u c -- c-addr+1 u-1 c )
        DQ ROT          ; u c c-addr
        DQ oneplus      ; u c c-addr
        DQ ROT          ; c c-addr u
        DQ oneminus     ; c c-addr u
        DQ ROT          ; c-addr u c
        DQ EXIT
        CtoL(toNUMBER)

        DQ 3
        DQ 'm*/'        ; std1994 double
Mstarslash:
        DQ stdexe
        ; M*/ ( d1 n1 +n2 -- d2 )
        DQ twoOVER
        DQ twoOVER      ; (d1 n1 n2 d1 n1 n2)
        DQ DROP         ; (d1 n1 n2 d1 n1)
        DQ XOR          ; (d1 n1 n2 dsign)
        ; dsign is a double with the correct sign
        ; and otherwise arbitrary digits
        DQ twoROT       ; (n1 n2 dsign d1)
        DQ DABS         ; (n1 n2 dsign ud1)
        DQ twoROT       ; (dsign ud1 n1 n2)
        DQ SWAP         ; (dsign ud1 n2 n1)
        DQ fABS         ; (dsign ud1 n2 u1)
        DQ SWAP         ; (dsign ud1 u1 n2)
        DQ UMstarslashMOD  ; (dsign ud r)
        DQ DROP         ; (dsign ud)
        DQ ROT          ; (x ud sign)
        DQ Dplusminus   ; (x d)
        DQ ROT          ; (d x)
        DQ DROP
        DQ EXIT
        CtoL(Mstarslash)

        DQ 3
        DQ 'ud*'
UDstar:
        DQ stdexe
        ; UD* ( ud u -- ud-product )
        DQ SWAP, OVER   ; ul u um u
        DQ star         ; ul u mprod
        DQ toR          ; ul u
        DQ UMstar       ; ud
        DQ z, Rfrom     ; ud 0 mprod
        DQ Dplus        ; udprod
        DQ EXIT
        CtoL(UDstar)

        DQ 7
        DQ 'um*/mod'
UMstarslashMOD:
        DQ $+8
        ; UM*/MOD ( ud1 u1 +n2 -- ud2 +n3 )
        ; Same as M*/ but unsigned everywhere, and leaving MOD.
        mov rax, [rbp-32]       ; least sig of ud1
        mov r8, [rbp-24]        ; most sig of ud1
        mov r10, [rbp-16]       ; u1
        ; Compute triple-cell intermediate result in
        ; (most sig) r13, r14, r15 (least sig)
        ; rax * u1 -> r14:r15
        mul r10
        mov r15, rax
        mov r14, rdx
        ; r8 * u1 added to r13:r14
        mov rax, r8
        mul r10
        add r14, rax
        adc rdx, 0
        mov r13, rdx
        ; triple-cell product now in r13:r14:r15
        mov r10, [rbp-8]        ; r10 now divisor
        ; Credit to LaFarr http://www.zyvra.org/lafarr/math/intro.htm
        ; for this multiword division.
        mov rdx, 0
        mov rax, r13
        div r10
        mov r13, rax
        mov rax, r14
        div r10
        mov r14, rax
        mov rax, r15
        div r10
        mov r15, rax
        ; Deposit result
        sub rbp, 8
        mov [rbp-24], r15
        mov [rbp-16], r14
        mov [rbp-8], rdx
        jmp next
        CtoL(UMstarslashMOD)

        DQ 3
        DQ '.x2'
dotx2:
        DQ stdexe
        ; .x2 ( u -- ) print 2 hex digits
        DQ z, LIT, 16   ; ud 16
        DQ UMslashMOD   ; lsd msd
        DQ LIT, 15, AND ; lsd msd
        DQ DIGIT, EMIT
        DQ DIGIT, EMIT
        DQ fBL, EMIT
        DQ EXIT
        CtoL(dotx2)

        DQ 4
        DQ 'dump'
DUMP:
        DQ stdexe
        ; DUMP ( addr u -- )
        DQ OVER         ; addr u addr
        DQ PLUS         ; addr limit
.l:
        DQ OVER, OVER
        DQ Ulessthan    ; addr limit bf
        DQ ZEROBRANCH
        DQ (.x-$)
        DQ OVER         ; addr limit addr
        DQ Cfetch
        DQ dotx2
        DQ SWAP         ; limit addr
        DQ oneplus
        DQ SWAP         ; addr+1 limit
        DQ BRANCH
        DQ -($-.l)
.x:
        DQ DROP, DROP
        DQ EXIT
        CtoL(DUMP)

        DQ 6
        DQ 'tcgets'
TCGETS:
        DQ stdexe
        ; TCGETS ( fd p -- res )
        ; ioctl(fd, TCGETS, p) call
        ; TCGETS according to /usr/include/asm-generic/ioctls.h
        DQ LIT, 0x5401  ; fd p 0x5401
        DQ SWAP         ; fd 0x5401 p
        DQ LIT, 16      ; fd 0x5401 p 16
        DQ SYSCALL3     ; res
        DQ EXIT
        CtoL(TCGETS)

        DQ 6
        DQ 'tcsets'
TCSETS:
        DQ stdexe
        ; TCSETS ( fd p -- res )
        ; ioctl(fd, TCSETS, p) call
        ; TCSETS according to /usr/include/asm-generic/ioctls.h
        DQ LIT, 0x5402  ; fd p 0x5402
        DQ SWAP         ; fd 0x5402 p
        DQ LIT, 16      ; fd 0x5402 p 16
        DQ SYSCALL3     ; res
        DQ EXIT
        CtoL(TCSETS)

        DQ 7
        DQ 'tcgetsv'
TCGETSV:
        DQ stdvar
.b:
        TIMES 36 DB '@'
        CtoL(TCGETSV)

        DQ 6
        DQ 'isatty'
ISATTY:
        DQ stdexe
        ; ISATTY ( u-fd -- flag )
        ;   True if file descriptor u-fd refers to a TTY.
        DQ HERE         ; dummy buffer
        DQ TCGETS
        DQ zequals      ; 0 is success; convert to true/false
        DQ EXIT
        CtoL(ISATTY)

        DQ 4
        DQ 'base'       ; std1983
BASE:   DQ stdvar
abase:  DQ 10
        CtoL(BASE)

        DQ 5
        DQ 'digit'
DIGIT:  DQ stdexe
        ; DIGIT ( n -- ascii )
        ; convert digit (0 to 15) to ASCII
        ; 0 -> 48
        ; 10 -> 65
        DQ LIT, 9       ; (n 9)
        DQ OVER         ; (n 9 n)
        DQ lessthan     ; (n bf)
        DQ ZEROBRANCH
        DQ (.l-$)
        DQ LIT, 7
        DQ PLUS
.l:     DQ LIT, '0'
        DQ PLUS
        DQ EXIT
        CtoL(DIGIT)

        DQ 2
        DQ 'bl'         ; std1994
fBL:    DQ stdexe
        DQ LIT, ' '
        DQ EXIT
        CtoL(fBL)

        DQ 5
        DQ 'ftype'
FTYPE:
        DQ stdexe
        ; FTYPE ( addr +n fd -- )
        ; Type out string on file descriptor fd.
        DQ ROT, ROT     ; fd addr n
        DQ LIT, sys_write
        DQ SYSCALL3
        DQ DROP
        DQ EXIT
        CtoL(FTYPE)

        DQ 4
        DQ 'type'       ; std1983
TYPE:   DQ stdexe
        ; TYPE ( addr +n -- )
        DQ LIT, 1       ; addr n 1      ; stdout
        DQ FTYPE
        DQ EXIT
        CtoL(TYPE)

        DQ 5
        DQ 'femit'
FEMIT:
        DQ stdexe
        ; FEMIT ( ch fd -- )
        ; Emit character on file descriptor fd.
        DQ SWAP         ; fd ch
        DQ LIT, emitbuf ; fd ch addr
        DQ Cstore
        DQ LIT, emitbuf ; fd addr
        DQ LIT, 1       ; fd addr 1
        DQ ROT
        DQ FTYPE
        DQ EXIT
        CtoL(FEMIT)

        DQ 4
        DQ 'emit'       ; std1994
EMIT:
        DQ stdexe
        ; EMIT ( ch -- )
        DQ LIT, 1       ; ch 1
        DQ FEMIT
        DQ EXIT
        CtoL(EMIT)

        DQ 2
        DQ 'cr'         ; std1994
CR:
        DQ stdexe
        ; CR ( -- )
        DQ LIT, 10      ; POSIX
        DQ EMIT
        DQ EXIT
        CtoL(CR)

        DQ 1
        DQ '='          ; std1983
equals: DQ $+8
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        sub rax, rcx
        sub rax, 1
        sbb rax, rax
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        CtoL(equals)

        DQ 2
        DQ '0='         ; std1983
zequals:
        DQ $+8
        ; 0= (0 -- -1)
        ;    (x -- 0) when x is not 0
        ; true is all bits 1: -1
        mov rax, [rbp-8]
        sub rax, 1      ; is-zero now in Carry flag
        sbb rax, rax    ; C=0 -> 0; C=1 -> -1
        mov [rbp-8], rax
        jmp next
        CtoL(zequals)

        DQ 2
        DQ '0<'         ; std1983
zless:  DQ $+8
        ; 0< (n -- true) when n < 0
        ;    (n -- false) otherwise
        mov rax, [rbp-8]
        ; Shift sign bit into carry flag.
        shl rax, 1
        ; Convert carry flag to Forth boolean.
        sbb rax, rax
        mov [rbp-8], rax
        jmp next
        CtoL(zless)

        DQ 1
        DQ '<'          ; std1983
lessthan:
        DQ $+8
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        xor rdx, rdx
        cmp rax, rcx    ; V iff rax < rcx
        setl dl         ; :todo: seems super clumsy
        neg rdx
        sub rbp, 8
        mov [rbp-8], rdx
        jmp next
        CtoL(lessthan)

        DQ 2
        DQ 'u<'
Ulessthan:
        DQ $+8
        ; < ( u1 u2 -- flag )
        ; flag is -1 (TRUE) if u1 < u2;
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        cmp rax, rcx    ; C iff B > A
        sbb rax, rax    ; -1 iff B > A
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        CtoL(Ulessthan)

        DQ 6
        DQ 'negate'     ; std1983
NEGATE:
        DQ $+8
        mov rax, [rbp-8]
        neg rax
        mov [rbp-8], rax
        jmp next
        CtoL(NEGATE)

        DQ 3
        DQ 'abs'        ; std1983
fABS:   DQ $+8
        mov rax, [rbp-8]
        ; Convert carry flag to Forth boolean in rcx
        mov rcx, rax
        shl rcx, 1
        sbb rcx, rcx
        ; rcx is now 0 or -1
        ; If rcx is -1, negate the old-school way.
        xor rax, rcx
        sub rax, rcx
        mov [rbp-8], rax
        jmp next
        CtoL(fABS)

        DQ 1
        DQ '+'          ; std1983
PLUS:   DQ $+8
        ; + ( a b -- sum )
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        add rax, rcx
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        CtoL(PLUS)

        DQ 1
        DQ '-'          ; std1983
MINUS:  DQ $+8
        ; - ( a b -- difference )
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        sub rax, rcx
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        CtoL(MINUS)

        DQ 2
        DQ '1-'         ; std1983
oneminus:
        DQ $+8
        mov rax, [rbp-8]
        sub rax, 1
        mov [rbp-8], rax
        jmp next
        CtoL(oneminus)

        DQ 6
        DQ 'um/mod'     ; std1983
UMslashMOD:
        DQ $+8
        ; UM/MOD ( ud-dividend u-divisor -- u-r u-q )
        ; Note: Double Single -> Single Single.
        ; Dividend, least significant.
        mov rax, [rbp-24]
        ; Dividend, most significant.
        mov rdx, [rbp-16]
        ; Divisor
        mov rcx, [rbp-8]

        div rcx

        sub rbp, 8
        ; Deposit remainder.
        mov [rbp-16], rdx
        ; Deposit quotient.
        mov [rbp-8], rax
        jmp next
        CtoL(UMslashMOD)

        DQ 2
        DQ 'd+'         ; std1983
Dplus:  DQ $+8
        ; D+ ( d1 d2 -- d3 )
        mov rax, [rbp-32]       ; least significant part of augend
        mov rdx, [rbp-24]       ; most
        mov r8, [rbp-16]        ; least significant part of addend
        mov r9, [rbp-8]         ; most
        add rax, r8
        adc rdx, r9
        sub rbp, 16
        mov [rbp-16], rax
        mov [rbp-8], rdx
        jmp next
        CtoL(Dplus)

        DQ 4
        DQ 'true'       ; std1994
TRUE:   DQ stdexe
        DQ LIT, -1
        DQ EXIT
        CtoL(TRUE)

        DQ 5
        DQ 'false'      ; std1994
FALSE:  DQ stdexe
        DQ z
        DQ EXIT
        CtoL(FALSE)

        DQ 2
        DQ 'or'         ; std1983
OR:     DQ $+8
        ; OR ( a b -- bitwise-or )
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        or rax, rcx
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        CtoL(OR)

        DQ 3
        DQ 'and'        ; std1983
AND:    DQ $+8
        ; AND ( a b -- bitwise-and )
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        and rax, rcx
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        CtoL(AND)

        DQ 3
        DQ 'xor'        ; std1983
XOR:    DQ $+8
        ; XOR ( a b -- bitwise-xor )
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        xor rax, rcx
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        CtoL(XOR)

        DQ 6
        DQ 'invert'
INVERT:
        DQ stdexe
        DQ TRUE
        DQ XOR
        DQ EXIT
        CtoL(INVERT)

        DQ 3
        DQ 'bic'
BIC:
        DQ stdexe
        ; BIt Clear (name from Alpha Architecture)
        ; BIC ( na nb -- nc )
        DQ INVERT
        DQ AND
        DQ EXIT
        CtoL(BIC)

        DQ 2
        DQ 'cp'
CP:     DQ stdvar       ; https://www.forth.com/starting-forth/9-forth-execution/
        DQ dictfree
        CtoL(CP)

        DQ 4
        DQ 'here'       ; std1983
HERE:   DQ stdexe
        DQ CP
        DQ fetch
        DQ EXIT
        CtoL(HERE)

        DQ 3
        DQ '>in'        ; std1983
toIN:   DQ stdvar
atoIN:  DQ 0
        CtoL(toIN)

        DQ 6
        DQ 'source'     ; std1994
SOURCE:
        DQ stdexe
        ; :todo: Implement more input sources.
        DQ LIT, aIB
        DQ fetch
        DQ numberIB
        DQ fetch
        DQ EXIT
        CtoL(SOURCE)

        DQ 1
        DQ '!'          ; std1983
store:  DQ $+8
store0: ; ! ( w addr -- )
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        sub rbp, 16
        mov [rcx], rax
        jmp next
        CtoL(store)

        DQ 2
        DQ 'c!'         ; std1994
Cstore:
        DQ $+8
        ; C! ( ch buf -- )
        mov rax, [rbp-16]
        mov rdx, [rbp-8]
        sub rbp, 16
        mov [rdx], al
        jmp next
        CtoL(Cstore)

        DQ 1
        DQ '@'          ; std1983
fetch:  DQ $+8
        ; @ ( addr -- w )
        mov rax, [rbp-8]
        mov rax, [rax]
        mov [rbp-8], rax
        jmp next
        CtoL(fetch)

        DQ 2
        DQ 'c@'         ; std1983
Cfetch: DQ $+8
        ; C@ ( addr -- ch )
        mov rdx, [rbp-8]
        xor rax, rax
        mov al, [rdx]
        mov [rbp-8], rax
        jmp next
        CtoL(Cfetch)

        DQ 2
        DQ '+!'         ; std1983
plusstore:
        DQ stdexe
        ; ( w addr -- )
        DQ SWAP         ; (a w)
        DQ OVER         ; (a w a)
        DQ fetch        ; (a w b)
        DQ PLUS         ; (a s)
        DQ SWAP         ; (s a)
        DQ store
        DQ EXIT
        CtoL(plusstore)

        DQ 4
        DQ 'swap'       ; std1983
SWAP:   DQ $+8
        ; SWAP ( a b -- b a )
        mov rax, [rbp-16]
        mov rdx, [rbp-8]
        mov [rbp-16], rdx
        mov [rbp-8], rax
        jmp next
        CtoL(SWAP)

        DQ 5
        DQ '2swap'      ; std1994
twoSWAP:
        DQ $+8
        ; 2SWAP ( p q r s -- r s p q )
        ; Swap 2OS and 4OS
        mov rax, [rbp-32]
        mov rcx, [rbp-24]
        mov rdx, [rbp-16]
        mov rsi, [rbp-8]
        mov [rbp-32], rdx
        mov [rbp-24], rsi
        mov [rbp-16], rax
        mov [rbp-8], rcx
        jmp next
        CtoL(twoSWAP)

        DQ 4
        DQ '?dup'       ; std1983
qDUP:   DQ $+8
        ; ?DUP ( nz -- nz nz )  when not zero
        ;      ( 0 -- 0 )       when zero
        mov rax, [rbp-8]
        test rax, rax
        jz next
        jmp pushrax
        CtoL(qDUP)

        DQ 4
        DQ 'over'       ; std1983
OVER:   DQ $+8
        ; OVER ( a b -- a b a )
        mov rax, [rbp-16]
        jmp pushrax
        CtoL(OVER)

        DQ 5
        DQ '2over'      ; std1994
twoOVER:
        DQ $+8
        ; 2OVER ( p q r s -- p q r s p q )
        mov rcx, [rbp-32]
        mov rdx, [rbp-24]
        add rbp, 16
        mov [rbp-16], rcx
        mov [rbp-8], rdx
        jmp next
        CtoL(twoOVER)

        DQ 4
        DQ '2rot'       ; std1994 double ext
twoROT:
        DQ $+8
        ; 2ROT ( n o p q r s -- p q r s n o )
        mov rcx, [rbp-48]
        mov rdx, [rbp-40]
        mov r8, [rbp-32]
        mov r9, [rbp-24]
        mov [rbp-48], r8
        mov [rbp-40], r9
        mov r8, [rbp-16]
        mov r9, [rbp-8]
        mov [rbp-32], r8
        mov [rbp-24], r9
        mov [rbp-16], rcx
        mov [rbp-8], rdx
        jmp next
        CtoL(twoROT)

        DQ 3
        DQ 'd>s'        ; std1994 double
DtoS:   DQ stdexe
        DQ DROP
        DQ EXIT
        CtoL(DtoS)

        DQ 3
        DQ 'd+-'        ; acornsoft
Dplusminus:
        DQ $+8
        ; m+- (d n -- d)
        mov rcx, [rbp-16]
        mov rax, [rbp-8]
        sub rbp, 8
        ; Have operands got same sign?
        xor rax, rcx
        jns .x
        ; rcx has most significant single precision number.
        ; Put least sigfnificant single into rax.
        mov rax, [rbp-16]
        mov rdx, 0
        sub rdx, rax
        ; Negated least significant now in rdx.
        mov rax, 0
        sbb rax, rcx
        ; Megated most significant now in rax.
        mov [rbp-16], rdx
        mov [rbp-8], rax
.x:     jmp next
        CtoL(Dplusminus)

        DQ 4
        DQ 'dabs'       ; std1994 double
DABS:   DQ stdexe
        ; DABS ( d -- ud )
        DQ LIT, 7       ; Arbitrary, should be positive.
        DQ Dplusminus
        DQ EXIT
        CtoL(DABS)

        DQ 4
        DQ 'drop'       ; std1983
DROP:   DQ $+8
        ; DROP ( a -- )
        sub rbp, 8
        jmp next
        CtoL(DROP)

        DQ 3
        DQ 'nip'        ; std1994 core-ext
NIP:    DQ $+8
        ; NIP ( a b -- b )
        mov rax, [rbp-8]
        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        CtoL(NIP)

        DQ 5
        DQ 'allot'      ; std1983
ALLOT:  DQ stdexe
        ; allot ( w -- )
        DQ CP
        DQ plusstore
        DQ EXIT
        CtoL(ALLOT)

        DQ 1
        DQ ','          ; std1983
comma:  DQ stdexe
        ; , ( w -- )
        DQ HERE
        DQ LIT, 8
        DQ ALLOT
        DQ store
        DQ EXIT
        CtoL(comma)

        DQ 7 | Immediate
        DQ 'literal'    ; std1983
LITERAL:
        DQ stdexe
        DQ LIT, LIT     ; haha, weird or what?
        DQ comma
        DQ comma
        DQ EXIT
        CtoL(LITERAL)

        DQ 5
        DQ 'cmove'      ; std1983
CMOVE:
        DQ $+8
        ; CMOVE ( from to u -- )
cmove0:
        mov rsi, [rbp-24]
        mov rdi, [rbp-16]
        mov rcx, [rbp-8]
        sub rbp, 24
        mov rdx, 0
.l:     cmp rcx, rdx
        jz next
        mov al, [rsi+rdx]
        mov [rdi+rdx], al
        inc rdx
        jmp .l
        CtoL(CMOVE)

        DQ 6
        DQ 'cmove>'
CMOVEup:
        DQ $+8
        ; CMOVE> ( from to u -- )
        mov rsi, [rbp-24]
        mov rdi, [rbp-16]
        mov rcx, [rbp-8]
        sub rbp, 24
.l:     cmp rcx, 0
        jz next
        dec rcx
        mov al, [rsi+rcx]
        mov [rdi+rcx], al
        jmp .l
        CtoL(CMOVEup)

        DQ 4
        DQ 'fill'       ; std1983
FILL:
        DQ $+8
        ; FILL ( c-addr u char -- )
        mov rdi, [rbp-24]
        mov rcx, [rbp-16]
        mov rax, [rbp-8]
        sub rbp, 24
        mov rdx, 0
.l:
        cmp rcx, rdx
        jz next
        mov [rdi+rdx], al
        inc rdx
        jmp .l
        CtoL(FILL)

        DQ 3
        DQ 'min'        ; std1983
MIN:
        DQ stdexe
        DQ OVER
        DQ OVER
        DQ greaterthan  ; a b flag
        DQ ZEROBRANCH
        DQ .s-$
        DQ SWAP
.s:
        DQ DROP
        DQ EXIT
        CtoL(MIN)

        DQ 7
        DQ '*create'
starCREATE:
        DQ stdexe
        ; *CREATE ( addr u -- )
        ; Create dictionary entry for word left on stack.
        ; Link Field Address
        DQ HERE         ; ( addr u lfa)
        ; Rotate the LFA out of the way for now
        DQ ROT, ROT     ; ( lfa addr u )
        ; Compile Link Field
        DQ LIT, DICT
        DQ fetch
        DQ comma

        ; Compile Name Field.
        ; Name Length.
        DQ DUP
        DQ comma
        ; Name String
        DQ LIT, 8       ; ( lfa addr u 8 )
        DQ MIN          ; ( lfa addr u|8 )
        DQ HERE         ; ( lfa addr u|8 here )
        DQ SWAP         ; ( lfa addr here u|8 )
        DQ CMOVE        ; ( lfa )
        DQ LIT, 1
        DQ CELLS        ; ( lfa cc )
        DQ ALLOT

        ; Compile Code Field
        DQ LIT, stdvar
        DQ comma
        ; Update Dictionary pointer
        DQ LIT, DICT    ; ( lfa &dict )
        DQ store
        DQ EXIT
        CtoL(starCREATE)

        DQ 6
        DQ 'create'
CREATE:
        DQ stdexe
        DQ PARSEWORD    ; ( addr u )
        DQ starCREATE
        DQ EXIT
        CtoL(CREATE)

        DQ 8
        DQ 'codedoes'
CODEDOES:
        DQ stdexe
        ; CODEDOES ( -- addr n )
        ; (used internally by DOES>)
        ; Push the string that is the machine code
        ; that is pointed to by the CFA
        ; of the word that DOES> changes.
        DQ LIT, doestarget
        DQ LIT, doestargetlen
        DQ EXIT
doestarget:
        ; This sequence, doestarget, has the property that
        ; it is _absolute_.
        ; Meaning that it can be copied anywhere, and
        ; still has the same behaviour.
        jmp [.n+0]
.n:
        DQ pushparam
doestargetlen EQU $ - doestarget

pushparam:
        ; Push the parameter field onto the stack.
        ; This machine code is executed by
        ; words that have been modified by DOES>.
        ; And it is reached via
        ; the absolute relocatable sequence `doestarget`.
        ; It transfers control to the threaded code following
        ; the call to this code.
        ; Push CODEPOINTER.
        mov [r12], rbx
        add r12, 8
        ; Fix THIS.
        add rdx, 8
        mov [rbp], rdx
        add rbp, 8
        lea rbx, [rax+doestargetlen]
        jmp next
        CtoL(CODEDOES)

        DQ 5
        DQ '>body'      ; std1983
toBODY: DQ stdexe
.body:  DQ LIT, (.body-toBODY)  ; 8, basically
        DQ PLUS
        DQ EXIT
        CtoL(toBODY)

        DQ 5
        DQ 'body>'      ; std1983[harris]
fromBODY:
        DQ stdexe
.body:  DQ LIT, (.body-fromBODY)        ; 8, basically
        DQ MINUS
        DQ EXIT
        CtoL(fromBODY)

nffromNAME:
        DQ 5
        DQ 'name>'      ; std1983[harris]
fromNAME:
        DQ stdexe
        DQ LIT, (fromNAME-nffromNAME)   ; 16, basically
        DQ PLUS
        DQ EXIT
        CtoL(fromNAME)

        DQ 5
        DQ 'state'      ; std1983
STATE:  DQ stdvar
stateaddr:
        DQ 0
        CtoL(STATE)

        DQ 1
        DQ ']'          ; std1983
ket:    DQ stdexe
        DQ LIT, 1
        DQ STATE
        DQ store
        DQ EXIT
        CtoL(ket)

        DQ 1
        DQ ':'          ; std1983
colon:  DQ stdexe
        DQ CREATE
        DQ LIT, stdexe
        DQ HERE
        DQ fromBODY
        DQ store
        DQ ket
        DQ EXIT
        CtoL(colon)

        DQ 1 | Immediate
        DQ ';'          ; std1983
semicolon:
        DQ stdexe
        DQ LIT, EXIT
        DQ comma
        ; :todo: check compiler safety
        DQ z
        DQ STATE
        DQ store
        DQ EXIT
        CtoL(semicolon)

        DQ 4
        DQ 'exit'       ; std1983
EXIT:   DQ $+8
        sub r12, 8
        mov rbx, [r12]
        jmp next
        CtoL(EXIT)

        DQ 2
        DQ '>r'         ; std1983
toR:    DQ $+8
        mov rax, [rbp-8]
        mov [r12], rax
        add r12, 8
        sub rbp, 8
        jmp next
        CtoL(toR)

        DQ 3
        DQ '2>r'        ; std1994 core-ext
twotoR:
        DQ $+8
        ; 2>R  ( x1 x2 -- )  ( r: -- x1 x2 )
        mov rax, [rbp-16]
        mov rcx, [rbp-8]
        add r12, 16
        mov [r12-16], rax
        mov [r12-8], rcx
        sub rbp, 16
        jmp next
        CtoL(twotoR)

        DQ 2
        DQ 'r>'         ; std1983
Rfrom:  DQ $+8
        mov rax, [r12-8]
        sub r12, 8
        jmp pushrax
        CtoL(Rfrom)

        DQ 2
        DQ 'r@'         ; std1983
Rfetch: DQ $+8
        mov rax, [r12-8]
        jmp pushrax
        CtoL(Rfetch)

        DQ 3
        DQ '2r@'        ; std1994 core-ext
twoRfetch:
        DQ $+8
        ; 2R@  ( -- x1 x2 )  ( r: x1 x2 -- x1 x2 )
        mov rcx, [r12-16]
        mov rax, [r12-8]
        add rbp, 8
        mov [rbp-8], rcx
        jmp pushrax
        CtoL(twoRfetch)

        DQ 8
        DQ 'findword'
FINDWORD:
        DQ $+8
        ; search and locate string in dictionary
        ; (similar interface to SEARCH-WORDLIST)
        ; FINDWORD ( c-addr u -- xt 1 ) found immediate
        ;          ( c-addr u -- xt -1 ) found non-immediate
        ;          ( c-addr u -- 0 ) not found
        mov rax, DICT           ; Link to most recent word
        ; rax points to Link Field
        ; (that points to the next word in the dictionary).
.loop:  mov rax, [rax]
        test rax, rax
        jz .empty
        mov r13, [rbp-8]        ; length
        mov rcx, [rbp-16]       ; pointer
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
        mov [rbp-16], rax
        ; std1983 requires -1 (true) for non-immediate word,
        ; and 1 for immediate word.
        ; Flags (rdx) is 0 for non-immediate; 2 for immediate.
        ; So we can subtract 1.
        sub rdx, 1
        mov [rbp-8], rdx
        jmp next
.empty:
        sub rbp, 8
        mov qword [rbp-8], 0
        jmp next
        CtoL(FINDWORD)

        DQ 4
        DQ 'quit'       ; std1983
QUIT:   DQ reset
        CtoL(QUIT)

        DQ 5
        DQ 'abort'      ; std1983
ABORT:  DQ dreset
        CtoL(ABORT)

        DQ 6
        DQ 'sm/rem'     ; std1994
SMslashREM:
        DQ $+8
        ; SM/REM ( d-dividend n-divisor -- n-quotient n-remainder )
        mov rax, [rbp-24]
        mov rdx, [rbp-16]
        mov rcx, [rbp-8]

        idiv rcx

        sub rbp, 8
        mov [rbp-16], rdx
        mov [rbp-8], rax
        jmp next
        CtoL(SMslashREM)

        DQ 1
        DQ '0'
z:      DQ stdexe
        DQ LIT, 0
        DQ EXIT
        CtoL(z)

        DQ 6
        DQ 'lshift'
LSHIFT:
        DQ $+8
        mov rax, [rbp-16]
        mov rcx, [rbp-8]

        shl rax, cl

        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        CtoL(LSHIFT)

        DQ 6
        DQ 'rshift'
RSHIFT:
        DQ $+8
        mov rax, [rbp-16]
        mov rcx, [rbp-8]

        shr rax, cl

        sub rbp, 8
        mov [rbp-8], rax
        jmp next
        CtoL(RSHIFT)

        DQ 2
        DQ 'm*'         ; std1994
Mstar:
        DQ $+8
        ; m* ( n1 n2 -- d )
        mov rax, [rbp-16]       ; multiplier
        mov rdx, [rbp-8]        ; multiplicand

        imul rdx

        mov [rbp-16], rax
        mov [rbp-8], rdx
        jmp next
        CtoL(Mstar)

        DQ 3
        DQ 'um*'        ; std1994
UMstar:
        DQ $+8
        ; um* ( u1 u2 -- ud )
        mov rax, [rbp-16]       ; multiplier
        mov rdx, [rbp-8]        ; multiplicand

        mul rdx

        mov [rbp-16], rax
        mov [rbp-8], rdx
        jmp next
        CtoL(UMstar)

        DQ 1
        DQ '*'          ; std1983
star: DQ stdexe
        ; * ( n1 n2 -- n3 )
        DQ Mstar
        DQ DROP
        DQ EXIT
        CtoL(star)

        DQ 2
        DQ '0>'         ; std1983
zgreater:
        DQ stdexe
        ; 0> (n -- b)
        DQ NEGATE
        DQ zless
        DQ EXIT
        CtoL(zgreater)

        DQ 2
        DQ '1+'         ; std1983
oneplus:
        DQ stdexe
        DQ LIT, 1
        DQ PLUS
        DQ EXIT
        CtoL(oneplus)

        DQ 2
        DQ '2+'         ; std1983
twoplus:
        DQ stdexe
        DQ LIT, 2
        DQ PLUS
        DQ EXIT
        CtoL(twoplus)

        DQ 2
        DQ '2-'         ; std1983
twominus:
        DQ stdexe
        DQ LIT, 2
        DQ MINUS
        DQ EXIT
        CtoL(twominus)

        DQ 1
        DQ '>'          ; std1983
greaterthan:
        DQ stdexe
        DQ SWAP
        DQ lessthan
        DQ EXIT
        CtoL(greaterthan)

        DQ 9
        DQ 'immediat'   ; std1983
IMMEDIATE:
        DQ stdexe
        DQ LAST         ; (addr)
        DQ DUP          ; (addr addr)
        DQ fetch        ; (addr length)
        DQ LIT
        DQ Immediate    ; (addr length immflag)
        DQ OR           ; (addr lengthflag)
        DQ SWAP         ; (l addr)
        DQ store
        DQ EXIT
        CtoL(IMMEDIATE)

        DQ 4
        DQ 'last'       ; Acornsoft
LAST:   DQ stdexe
        DQ LIT, DICT
        DQ fetch
        DQ LIT, 8       ; L>NAME
        DQ PLUS
        DQ EXIT
        CtoL(LAST)

        DQ 5
        DQ 'cells'      ; std1994
CELLS:  DQ stdexe
        DQ LIT, 8
        DQ star
        DQ EXIT
        CtoL(CELLS)

        DQ 5
        DQ 'cell+'      ; std1994
CELLplus:
        DQ stdexe
        DQ LIT, 1
        DQ CELLS
        DQ PLUS
        DQ EXIT
        CtoL(CELLplus)

        DQ 7
        DQ 'aligned'    ; std1994
ALIGNED:
        DQ stdexe
        ; ALIGNED ( addr -- a-addr )
        DQ LIT, 7
        DQ PLUS
        DQ LIT, 7
        DQ BIC
        DQ EXIT
        CtoL(ALIGNED)

        DQ 7 | Immediate
        DQ 'SLITERAL'   ; std1994 string
SLITERAL:
        DQ stdexe
        ; SLITERAL ( addr u -- )
        ; A string is compiled into the current word:
        ;    +--------------+
        ;    |  BRANCH      |
        ;    +--------------+
        ;    |  offset to T |
        ;    +--------------+
        ; A: |  copy of     |
        ;    |  string      |
        ;    |  ...aligned  |
        ;    +--------------+
        ; T: |  LIT         |
        ;    +--------------+
        ;    |  A           |
        ;    +--------------+
        ;    |  LIT         |
        ;    +--------------+
        ;    |  u           |
        ;    +--------------+
        DQ LIT, BRANCH, comma
        DQ DUP          ; ( c-addr u u )
        DQ CELLplus     ; ( c-addr u v )
        DQ ALIGNED      ; ( c-addr u v' )
        DQ comma        ; ( c-addr u )
        DQ HERE, SWAP   ; ( c-addr here u )
        DQ DUP, ALIGNED ; ( c-addr here u u' )
        DQ ALLOT        ; ( c-addr here u )
        DQ OVER, LITERAL
        DQ DUP, LITERAL
        DQ CMOVE
        DQ EXIT
        CtoL(SLITERAL)

        DQ 2 | Immediate
        DQ 's"'         ; std1994
Squote:
        DQ stdexe
        ; ( "ccc<quote>" -- ) runtime: ( -- addr u )
        DQ LIT, '"'
        DQ PARSE        ; ( c-addr u )
        DQ SLITERAL
        DQ EXIT
        CtoL(Squote)

        DQ 6 | Immediate
        DQ 'abort"'     ; std1983
ABORTquote:
        DQ stdexe
        DQ LIT, ZEROBRANCH, comma
        DQ HERE         ; ( here )
        DQ TRUE, comma
        DQ Squote
        DQ LIT, TYPE, comma
        DQ LIT, ABORT, comma
        DQ HERE         ; ( addr here )
        DQ OVER         ; ( addr here addr )
        DQ MINUS        ; ( addr offset )
        DQ SWAP         ; ( offset addr )
        DQ store
        DQ EXIT
        CtoL(ABORTquote)

        DQ 2
        DQ '<>'         ; std1994
notequals:
        DQ stdexe
        ; <> (a b -- -1)
        ;    (a a -- 0)
        DQ XOR
        DQ zequals
        DQ zequals
        DQ EXIT
        CtoL(notequals)

        DQ 4
        DQ 'inch'
INCH:
        DQ stdexe
        ; INCH ( u -- char TRUE ) when valid character
        ;      ( u -- FALSE FALSE ) when n out of range
        ; Fetch the next character in the parse area.
        ; If u is less than the number of characters in the
        ; input buffer, push the character at that position
        ; and the TRUE flag.
        ; Otherwise push FALSE FALSE.
        DQ DUP, SOURCE  ; u u s-addr u'
        DQ ROT          ; u s-addr u' u
        DQ greaterthan  ; u s-addr flag
        DQ ZEROBRANCH
        DQ .else-$
        DQ PLUS         ; addr
        DQ Cfetch       ; char
        DQ TRUE         ; char TRUE
        DQ BRANCH
        DQ .then-$
.else:
        DQ DROP, DROP   ;
        DQ z, z         ; FALSE FALSE
.then:
        DQ EXIT
        CtoL(INCH)

        DQ 6
        DQ 'partok'
PARTOK:
        DQ stdexe
        ; PARTOK ( base limit -- c-addr u )
        ; Parse a token from the SOURCE input;
        ; characters that are WITHIN base limit form the
        ; token (whose limits are returned as c-addr u).
        ; The parse area is scanned until a terminating character
        ; that is not WITHIN base limit.
        ; >IN is advanced until:
        ;   either the end of parse area is reached; or,
        ;   it points to a non-token character.
        ;
        ; This is used by PARSE and by
        ; both the skip and the scan phases of PARSE-WORD.
        ; Because of the way WITHIN is defined to work,
        ; if base < limit, then characters between
        ; base and limit will be included in the token.
        ; if limit < base, then characters between
        ; base and limit will delimit the token.
        ;
        ; In following the code,
        ; recall `>in` is an offset from the source address.
        ; `o` is the original value of `>in`, saved at the beginning
        ; of this word and tucked away on the return stack.
        DQ toIN, fetch  ; base limit o
        DQ toR          ; base limit  r: o
        DQ twotoR       ; r: o base limit
        DQ toIN, fetch  ; >in
.ch:
        DQ DUP, INCH    ; >in char flag-valid
        ; To avoid an IF (ZEROBRANCH),
        ; proceed to test the char even if invalid.
        ; Both tests are combined using AND.
        DQ SWAP         ; >in flag-valid char
        DQ twoRfetch    ; >in flag-valid char base limit
        DQ WITHIN       ; >in flag-valid flag-within
        DQ AND          ; >in flag
        DQ ZEROBRANCH
        DQ .got-$
        ; increment >in
        DQ oneplus      ; >in'
        DQ BRANCH
        DQ -($-.ch)
.got:
        ; >in  r: o base limit
        DQ DUP
        DQ toIN, store
        DQ Rfrom, DROP  ; >in  r: o base
        DQ Rfrom, DROP  ; >in  r: o
        ; convert two indexes into addr u form
        DQ Rfetch       ; >in o
        DQ MINUS        ; u
        DQ Rfrom        ; u o
        DQ SOURCE       ; u o s-addr u
        DQ DROP         ; u o s-addr
        DQ PLUS         ; u c-addr
        DQ SWAP         ; c-addr u
        DQ EXIT
        CtoL(PARTOK)

        DQ 3
        DQ 'in+'
INplus:
        DQ stdexe
        ; IN+ ( -- )
        ; Advance >in by one character,
        ; unless end of parse area is reached.
        DQ toIN, fetch  ; >in
        DQ oneplus      ; >in'
        DQ SOURCE, NIP  ; >in' u
        DQ MIN
        DQ toIN, store
        DQ EXIT
        CtoL(INplus)

        DQ 5
        DQ 'parse'      ; std1994
PARSE:
        DQ stdexe
        ; ( char -- c-addr u )
        DQ DUP          ; char char
        DQ oneplus      ; base limit
        DQ SWAP         ; limit base
        DQ PARTOK
        DQ INplus
        DQ EXIT
        CtoL(PARSE)

        DQ 10
        DQ 'parse-wo'   ; suggested by std1994 A.6.2.2008
PARSEWORD:
        DQ stdexe
        DQ z
        DQ LIT, 33
        DQ SKIP
        DQ LIT, 33
        DQ z
        DQ PARTOK
        DQ INplus
        DQ EXIT
        CtoL(PARSEWORD)

        DQ 4
        DQ 'skip'
SKIP:
        DQ stdexe
        ; SKIP ( base limit -- )
        ; Skip over the initial portion of the parse area that
        ; consists of characters WITHIN base limit.
        ; >IN is advanced until:
        ;   either the end of parse area is reached; or,
        ;   it points to a non-skippable character.
        DQ PARTOK       ; addr u
        DQ DROP, DROP
        DQ EXIT
        CtoL(SKIP)

        DQ 8
        DQ 'evaluate'
EVALUATE:
        DQ stdexe
        ; EVALUATE ( c-addr u -- ) also side effects
        ; Push ib >in #ib onto return stack.
        DQ IB, fetch
        DQ toR
        DQ toIN, fetch
        DQ toR
        DQ numberIB, fetch
        DQ toR

        ; u > #ib
        DQ numberIB
        DQ store
        ; c-addr > ib
        DQ IB
        DQ store
        ; 0 > >in
        DQ z
        DQ toIN
        DQ store
        DQ INTERPRETLINE

        ; pop ib >in #ib from return stack
        DQ Rfrom
        DQ numberIB, store
        DQ Rfrom
        DQ toIN, store
        DQ Rfrom
        DQ IB, store
        DQ EXIT
        CtoL(EVALUATE)

        DQ 6
        DQ 'branch'
BRANCH: DQ $+8
        ; Read the next word as a relative offset (in bytes);
        ; branch by adding offset to current CODEPOINTER.
        mov rcx, [rbx]
        lea rbx, [rbx + rcx]
        jmp next
        CtoL(BRANCH)

        DQ 7
        DQ '0branch'
ZEROBRANCH:
        DQ $+8
        ; Read the next word as a relative offset (in bytes);
        ; pop the TOS and test it;
        ; if it is 0 then
        ; branch by adding offset to current CODEPOINTER.
        mov rcx, [rbx]
        sub rbp, 8
        mov rax, [rbp]
        test rax, rax
        jz .branch
        add rbx, 8
        jmp next
.branch:
        lea rbx, [rbx + rcx]
        jmp next
        CtoL(ZEROBRANCH)

        DQ 7
        DQ '*stdexe'
starSTDEXE:
        DQ stdexe
        ; Push the address of `stdexe`
        DQ LIT, stdexe
        DQ EXIT
        CtoL(starSTDEXE)

        DQ 7
        DQ '*vreset'
starVRESET:
        DQ stdexe
        ; Push the address of the RESET vector.
        DQ LIT, avRESET
        DQ EXIT
        CtoL(starVRESET)

        DQ 7
        DQ 'useless'
USELESS:
        DQ stdvar
        CtoL(USELESS)

dictfree TIMES 8000 DQ 0

DICT:   CtoL(USELESS)

; (outer) Interpreter loop:
; Repeat:
;   issue prompt;
;   fill buffers from input (if cannot, exit);
;   interpret line
READLOOP:
        DQ stdexe
.line:
        DQ qPROMPT
        DQ REFILL
        DQ ZEROBRANCH
        DQ (.x-$)
        DQ INTERPRETLINE
        DQ BRANCH
        DQ -($ - .line)
.x:     DQ EXIT

; Repeat, until the input buffer is empty:
;   PARSEWORD: lex single word from input: creates a string.
;   FINDWORD: To convert from string to DICT entry.
;   qEXECUTE: execute / convert number / compile.

INTERPRETLINE:
        DQ stdexe
        ; Interpret successively parsed words,
        ; until there are no more to interpret.
.w:
        DQ PARSEWORD    ; c-addr u
        DQ DUP          ; c-addr u u
        DQ ZEROBRANCH
        DQ .x-$
        DQ OVER, OVER   ; c-addr u c-addr u
        DQ FINDWORD     ; c-addr u { 0 | xt n }
        DQ qEXECUTE
        DQ BRANCH
        DQ -($-.w)
.x:
        DQ DROP, DROP
        DQ EXIT

ipl:    DQ stdexe
        DQ vRESET
        DQ sysEXIT

qEXECUTE:
        DQ stdexe
        ; ( c-addr u 0 -- n ) push number
        ; ( c-addr u xt n -- ... ) execute word
        ; xt (execution token) is typically left by FINDWORD.
        ; if n is non zero then EXECUTE/COMPILE xt;
        ; otherwise try and handle number.
        DQ qDUP
        DQ ZEROBRANCH
        DQ .number-$
        ; c-addr u xt +-1
        DQ ROT, DROP    ; c-addr xt +-1
        DQ ROT, DROP    ; xt +-1
        ; immediate=1; non-immediate=-1
        ; compile if both a non-immediate word and compiling.
        DQ zless        ; xt flag
        DQ STATE, fetch ; xt flag compiling?
        DQ AND          ; xt compile?
        ; 0=execute; nz=compile
        DQ ZEROBRANCH
        DQ .exec-$
        DQ comma
        DQ EXIT
.exec:  ; ( xt )
        DQ EXECUTE
        DQ EXIT
.number:
        ; Handle as number.
        ; (c-addr u)
        DQ qNUMBER
        DQ ZEROBRANCH
        DQ (.abort-$)
        ; (n)
        DQ STATE, fetch ; n compiling?
        DQ ZEROBRANCH
        DQ .x-$
        DQ LITERAL
.x:
        DQ EXIT
.abort:
        DQ TYPE
        DQ LIT, .error
        DQ LIT, .errorlen
        DQ TYPE
        DQ QUIT
.error: DB ' ?', 10
.errorlen EQU $-.error


qNUMBER:
        DQ stdexe
        ; ?NUMBER ( c-addr u -- n true ) if convertible
        ;         ( c-addr u -- c-addr u false ) if not convertible
        DQ OVER, OVER   ; c-addr u c-addr u
        DQ scansign     ; c-addr u sign c-addr u
        DQ z, z         ; c-addr u sign c-addr u 0 0
        DQ twoSWAP      ; c-addr u sign 0 0 c-addr u
        DQ toNUMBER     ; c-addr u sign ud c-addr' u')
        DQ ZEROBRANCH
        DQ (.success-$)
        ; c-addr u sign ud a
        DQ DROP         ; c-addr u sign ud
        DQ DROP, DROP   ; c-addr u sign
        DQ DROP         ; c-addr u
        DQ FALSE        ; c-addr u false
        DQ EXIT
.success:
        ; c-addr u sign ud a
        DQ DROP         ; c-addr u sign ud
        DQ twoSWAP      ; c-addr ud u sign
        DQ NIP          ; c-addr ud sign
        DQ Dplusminus   ; c-addr d
        DQ DtoS         ; c-addr n      :todo: check range
        DQ NIP          ; n
        DQ TRUE         ; n true
        DQ EXIT

scansign:
        DQ stdexe
        ; ( addr u -- -1 addr' u' ) when leading '-' present
        ; ( addr u -- 0 addr u )    when leading '-' absent
        ; Scan an initial '-' sign at the beginning of a number.
        DQ DUP
        DQ ZEROBRANCH
        DQ .empty-$
        DQ SWAP         ; u addr
        DQ DUP
        DQ Cfetch       ; u addr ch
        DQ LIT, '-'
        DQ equals       ; u addr bf
        ; Note: here use fact that -1 is TRUE
        DQ ROT          ; addr bf u
        DQ OVER         ; addr bf u bf
        DQ PLUS         ; addr bf u'
        DQ ROT, ROT     ; u' addr bf
        DQ SWAP
        DQ OVER         ; u' bf addr bf
        DQ MINUS        ; u' bf addr'
        DQ ROT          ; bf addr' u'
        DQ EXIT
.empty: DQ z
        DQ ROT, ROT
        DQ EXIT

vRESET:
        DQ stdexe
        ; vectored reset
avRESET:
        DQ RUNRC
        DQ EXIT

; Input Buffer / Parse Area
; A fresh block, from OS, is read into ib1.
; IB + #IB <= IBLIMIT
; When a new line is required,
; the input block area is scanned until either:
; a line terminator is found; or,
; IBLIMIT is reached.
; If a line terminator is found then
; IB and #IB are updated, with no new input from OS.
; If IBLIMIT is reached then
; the valid portion of ib1 (from IB to IBLIMIT)
; is copied to the end of ib0, and
; a fresh block is read into ib1,
; updating IBLIMIT (and IB and #IB).
; If the freshly read block has length zero, then
; that marks EOF and REFILL returns FALSE.

numberIB:
        DQ stdvar
        ; Size of current input buffer.
anumberIB:
        DQ 0

IB:
        DQ stdvar
        ; address of current input buffer.
aIB:
        DQ ib1

IBLIMIT:
        DQ stdvar
        ; Pointer to one past last valid byte in input block.
aIBLIMIT:
        DQ ib1


SECTION .text
GLOBAL _start
_start:
dreset: ; ABORT jumps here (data reset)
        ; Initialise the model registers.
        mov rbp, stack
reset:  ; QUIT jumps here
        mov rcx, 0
        mov r12, returnstack
        mov rax, stateaddr
        mov qword [rax], 0
        mov rax, anumberIB
        mov qword [rax], 0
        mov rax, atoIN
        mov [rax], rcx
        mov rcx, ib1
        mov rax, aIB
        mov [rax], rcx
        mov rax, aIBLIMIT
        mov [rax], rcx

        ; Initialising RDX (aka THIS) and RBX (aka CODEPOINTER),
        ; so as to fake executing the Forth word IPL.
        mov rdx, vRESET
        mov rbx, ipl+16

stdexe:
        ; Stack CODEPOINTER onto continuation stack, then
        mov [r12], rbx
        add r12, 8
        ; Fetch new CODEPOINTER from THIS.
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

LIT:    DQ $+8
        mov rax, [rbx]
        add rbx, 8
        mov [rbp], rax
        add rbp, 8
        jmp next

COPYDOWN:
        DQ stdexe
        ; Copy region from (newly bumped) IB (to IBLIMIT)
        ; down to end of ib0.
        DQ LIT, ib1     ; ib1
        DQ IBLIMIT
        DQ fetch        ; ib1 iblimit
        DQ OVER         ; ib1 iblimit ib1
        DQ MINUS        ; ib1 u
        DQ DUP
        DQ NEGATE       ; ib1 u -u
        DQ IB
        DQ plusstore    ; check IB underflow here
        DQ OVER
        DQ OVER         ; ib1 u ib1 u
        DQ MINUS        ; ib1 u target
        DQ SWAP         ; ib1 target u
        DQ CMOVE        ;
        DQ EXIT

SCAN:
        DQ stdexe
        ; scan the input buffer, from IB to IBLIMIT,
        ; for a newline.
        ; SCAN ( -- p false ) newline found
        ;      ( -- iblimit true ) newline not found
        DQ IB
        DQ fetch        ; p
.begin:
        DQ IBLIMIT
        DQ fetch        ; p iblimit
        DQ OVER         ; p iblimit p
        DQ equals       ; p flag
        DQ ZEROBRANCH
        DQ .ch - $
        DQ TRUE
        DQ EXIT
.ch:
        ; p
        DQ DUP
        DQ Cfetch       ; p c
        DQ LIT, 10      ; p c 10
        DQ equals       ; p flag
        DQ ZEROBRANCH
        DQ .continue - $
        DQ FALSE
        DQ EXIT
.continue:
        ; p
        DQ oneplus
        DQ BRANCH
        DQ -($ - .begin)

BUMPIB:
        DQ stdexe
        ; Bump IB past the current record,
        ; and reset >IN to 0.
        ; IB has #IB added to it,
        ; and if it is still within IBLIMIT,
        ; and is positioned at a newline,
        ; IB is incremented past it.
        DQ z
        DQ toIN
        DQ store
        DQ numberIB     ; #ib
        DQ fetch
        DQ z
        DQ numberIB
        DQ store
        DQ IB           ; #ib ib
        DQ plusstore
        DQ IB
        DQ fetch
        DQ IBLIMIT
        DQ fetch        ; ib iblimit
        DQ equals
        DQ ZEROBRANCH
        DQ .bump - $
        DQ EXIT
.bump:
        DQ IB
        DQ fetch        ; ib
        DQ Cfetch       ; c
        DQ LIT, 10      ; c 10
        DQ equals
        DQ ZEROBRANCH
        DQ .then - $
        DQ LIT, 1
        DQ IB           ; 1 &ib
        DQ plusstore
.then:
        DQ EXIT

REFILL:
        DQ stdexe
        ; REFILL ( -- false ) no more input / end of file
        ;        ( -- true ) input available, see SOURCE
        ; see numberIB for description.
        DQ BUMPIB
        DQ SCAN         ; p flag
        DQ ZEROBRANCH
        DQ .scanned - $
        DQ DROP
        DQ COPYDOWN
        DQ READIB1
        ; If empty after reading, can't refill.
        DQ IBLIMIT, fetch       ; iblimit
        DQ IB, fetch            ; iblimit ib
        DQ equals
        DQ ZEROBRANCH
        DQ .then - $
        DQ FALSE
        DQ EXIT
.then:
        DQ SCAN         ; p flag
        ; Regardless of whether we found a newline,
        ; return the parse area.
        ; It must be the case that the last read block
        ; does not contain a newline.
        ; Either it is an unterminated final record
        ; (which we are going to accept),
        ; or there is a line longer than a block.
        ; The latter means that the line is too long,
        ; we parse it where the block falls and carry on.
        DQ DROP         ; p
.scanned:
        ; p marks next newline (or one past the end of the block)
        DQ IB, fetch            ; p ib
        DQ MINUS                ; u
        DQ numberIB             ; u &#ib
        DQ store
        DQ TRUE
        DQ EXIT

READIB1:
        DQ stdexe
        DQ LIT, ib1
        DQ IBLIMIT
        DQ store
        DQ LIT, 0       ; stdin
        DQ LIT, ib1
        DQ LIT, ibsize
        DQ SYSREAD
        DQ IBLIMIT
        DQ plusstore
        DQ EXIT

qPROMPT:
        DQ stdexe
        ; If interactive and the input buffer is empty,
        ; issue a prompt.
        DQ LIT, 0       ; stdin
        DQ ISATTY
        DQ zequals
        DQ ZEROBRANCH
        DQ .interactive - $
        DQ EXIT
.interactive:
        DQ toIN
        DQ fetch
        DQ numberIB
        DQ fetch
        DQ notequals
        DQ ZEROBRANCH
        DQ .then - $
        DQ EXIT
.then:
        DQ LIT, 2       ; stderr
        DQ LIT, prompt
        DQ LIT, promptlen
        DQ LIT, sys_write
        DQ SYSCALL3
        DQ DROP
        DQ EXIT

RC:
        DQ stdexe
        ; RC ( -- addr u )
        ; Push the address and length of the internally stored
        ; `rc.4` file.
        ; Ready for EVALUATE.
        DQ LIT, _binary_rc_4_start
        DQ LIT, _binary_rc_4_size
        DQ EXIT

RUNRC:
        DQ stdexe
        DQ LIT, READLOOP
        DQ LIT, avRESET
        DQ store
        DQ RC
        DQ EVALUATE
        ; We just changed the vectored reset to point to READLOOP,
        ; so this jumps to the READLOOP.
        DQ QUIT


