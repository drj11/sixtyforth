# A note about definition ordering

System initialisation consists of 2 phases.
The first phase is handled by the host OS:
the binary is mapped into memory and executed.
The second phase is the execution of system Forth boot script
(currently called `rc.4`).

At the end of the second phase,
the system is ready to execute the user application,
either interactively from `stdin`,
or from a script named on the command line.

The goal of the first phase is
to have enough of an interpreter working that
the Forth code from the second phase can be executed.

The second phase then contains the rest of the Forth system,
written in Forth.

The first phase is compiled into a system binary from
assembly files.
The assembly files include primitives defined in direct
assembly, and
primitives defined as threaded indirect code.

Primitives are defined in assembly for various reasons:
- use machine code instruction not otherwise available;
- speed;
- convenience;

Most of the threaded indirect code primitives
could be compiled from Forth
in the sense that
they have the same form in memory
that an equivalent Forth definition
would compile to.

They are not defined in Forth usually because
they form part of the interpreter / compiler and
so cannot be defined using the interpreter.

A few threaded indirect code primitives cannot be
compiled from Forth because either they have circular
dependencies or they use sneaky threaded code tricks.
The inner interpreter is capable of interpreting them,
but the compiler cannot produce them.
(is there an actual example?)

The Forth code for the second phase
is also compiled into the system binary
by using linker directives
to include the file text as a string in memory.
The first phase ends by jumping through the reset vector
which initially points to the `RUNRC` word
which is responsible for executing the second phase.

Copying the second phase code into the binary like this
means that file handling code can be taken out of the first phase.
This is possible because the SixtyForth binary
is made by an outside system: in this case,
the assembler and the linker.
