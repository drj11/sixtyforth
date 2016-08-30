The 16 registers are labelled RAX, RBX, RCX, RDX, then RBP, RSI,
RDI, RSP, and then R8 to R15.

`syscall` accepts system call number in %rax.
Return value is also in %rax.
The registers used to pass arguments for a syscall are:
RDI, RSI, RDX, R10, R8, R9

List of syscall numbers http://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/

gcc assembler, `as`, has destination on right.
`gdb` will use gcc convention: destination on right.

`nasm`, per platform convention, has destination on left.

Typical `nasm` workflow:

    nasm -f elf64 -l foo.lst foo.asm
    ld -s -o foo foo.o
