# SixtyForth

Notes on the design and implementation of SixtyForth.

## REFERENCES

[FORTH1983] The Forth-83 Standard:
http://forth.sourceforge.net/standard/fst83/

[FORTH1994] American National Standard for Information Systems —
Programming Language — Forth; ANSI X3.215-1994; 1994-03-24;
http://www.greenarraychips.com/home/documents/dpans94.pdf

[LOELIGER1981] "Threaded Interpretive Languages";
R. G. Loeliger; 1981

http://galileo.phys.virginia.edu/classes/551.jvn.fall01/primer.htm


## Register Conventions

Two stacks.
Data stack, and continuation stack.

Stacks grow upwards (TOS at numerically higher address).
Stacks are empty (so word at TOS has address REG-8).

RBP stack
RBX codepointer
R12 continuation stack
RDX for THIS


## Callable code

A standard Forth block looks like this:

    forthword:
        EXECODE
        CODEADDR1
        CODEADDR2
        ...

where `EXECODE` is the address of the machine code to call, and
`CODEADDR1` is the address of another executable block.
The addresses that are compiled (CODEADDR1 and so on)
are referred to in the Forth-83 standard as
"compilation addresses".
The compilation address of the above example word
is `forthword`.

What does the core interpreter cycle do?

In the general case, at entry to cycle,
the address of the current block is in the register THIS
(it's RDX, see above).

stdexe:
- push CODEPOINTER to continuation stack
- copy THIS to CODEPOINTER
- increment CODEPOINTER
next:
- fetch word at CODEPOINTER into THIS
- increment CODEPOINTER
- fetch word from THIS into TARGET
- jump to TARGET

At the end of a typical Forth block,
there is the word EXIT (defined by [FORTH1983]).
EXIT, when executed,
effectively does a function return.
It:

- pop Top Of Continuation Stack to CODEPOINTER
- goto next (in standard cycle)

A standard Forth block has an executable code address
followed by a sequence of dictionary words (see above).
Other prototypical blocks might be useful.

For executing machine code, we might have a block
that starts with an executable code address and
is followed by machine code.
The executable code address should be the address of the
subsequent word.

Variables are words that place an address on the stack.
In this implementation they have a code field of `stdvar`.
They deposit their parameter field address
(see `>BODY` and so on in [FORTH1983])
on the stack.

The word `CREATE` creates dictionary words
that have `stdvar` in their code field.

## Calling between

How do we call Forth from machine code, and vice versa?

## Standards

Mostly Forth-83 but expanded to 64-bit.
Where we need additional words,
these will come from ANSI FORTH.

Additional words to keep an eye on:

S>D
