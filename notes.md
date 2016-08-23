the 16 registers are labelled RAX, RBX, RCX, RDX, then RBP, RSI,
RDI, RSP, and then R8 to R15.

syscall accepts syscall number in %rax

gcc assembler, `as`, has destination on right.

`nasm`, per platform convention, has destination on left.

Typical `nasm` workflow:

    nasm -f elf64 -l foo.lst foo.asm
    ld -s -o foo foo.o
