# Assembler

The 16 registers are labelled RAX, RBX, RCX, RDX, then RBP, RSI,
RDI, RSP, and then R8 to R15.

`syscall` accepts system call number in %rax.
Return value is also in %rax.
The registers used to pass arguments for a syscall are:
RDI, RSI, RDX, R10, R8, R9.
`syscall` destroys RCX and R11.

List of syscall numbers http://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/

For operations involving load/store it is sometimes necessary to
give the data size.
Either by specifying a pseudoregister that is smaller is size,
or by explicitly specifying the size of the data.
For example, `mov [rdx], al` transfers a byte quantity.
`mov qword [rbp], len` transfers a quadword.


## Operand order

gcc assembler, `as`, has destination on right.
`gdb` uses gcc convention: destination on right.

`nasm` uses the same convention as the platform documentation,
and has destination on left.

## Tooling

Typical `nasm` workflow:

    nasm -f elf64 -l foo.lst foo.asm
    ld -s -o foo foo.o

