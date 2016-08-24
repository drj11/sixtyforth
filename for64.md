# For64

What might a callable fragment look like?

Typically we expect a standard Forth block to look something like
this:

    entrypoint
        EXECODE
        CODEADDR1
        CODEADDR2
        ...

where `EXECODE` is the address of the machine code to call, and
`CODEADDR1` is the address of another executable block.

What does the core interpreter cycle do?

In the general case, at entry to cycle,
the address of the current block is TOS.

- push CODEPOINTER to continuation stack
- pop TOS to CODEPOINTER
fetch:
- fetch word at CODEPOINTER onto new TOS
- increment CODEPOINTER
- keeping TOS, fetch word from TOS into TARGET
- jump to TARGET

What does RETURN do? I'm assuming there is a word compiled onto
the end of every block that does effectively a function return.
It should:

- drop TOS
- pop Top Of Continuation Stack to CODEPOINTER
goto fetch (in standard cycle)

A standard Forth block will have an executable code address
followed by a sequence of dictionary words;
for executing machine code, it might be useful to have
a block that starts with an executable code address and
is followed by machine code.

Such a Direct Block would look like this:

    DIRECTEXE
    machine code
    ...

the DIRECTEXE address would be the same for all such blocks.
It is a machine code routine that (jumps to the location 8+TOS):

    - pops TOS to TARGET
    - increments TARGET
    - jump to TARGET
