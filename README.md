# SixtyForth

[![Build Status](https://travis-ci.org/drj11/sixtyforth.svg?branch=master)](https://travis-ci.org/drj11/sixtyforth)

Welcome to SixtyForth, `64th` for short.

SixtyForth a 64-bit implementation of FORTH
written in 64-bit Intel assembler.

The SixtyForth language is
inspired by traditional Forth implementations,
but is also ultimately intended to
implement the ANSI Forth standard (1994 edition).

It does not yet implement the standard.

In this implementation a cell is 64-bits wide,
and signed integers are represented in 2's complement.

Input is mostly assumed to be in lower case.
Due to a whimsical quirk and
a fondness for 1980s style documentation,
various documents will use upper case
to refer to Forth words.
For example, `CREATE`.
But generally these should be
typed in lower case
in order to be
recognised by the SixtyForth system
(The standard requires that the uppercase forms are accepted,
but this is not yet implemented).

Input is accepted from the keyboard
(and interactively ^A ^E ^P ^N ^D ^K work);
from the command line (`./64th -c 'source type'`);
and from files (`./64th example/hw.64th`).

## Building SixtyForth and running it

Unpack the distribution from the magnetic tape.
Ensure NASM is installed.
Then:

    cd sixtyforth
    make

This creates the executable binary `64th`.
Run this to start using SixtyForth:

    ./64th

## Testing

The skeptical may want to run the tests first:

    make test

The tests are run using `urchin` which is an `npm` module.
`urchin` will be installed if `npm` works.

## Bugs, Issues, Fun

Please refer to github.

https://github.com/drj11/sixtyforth/

