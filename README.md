# SixtyForth

[![Build Status](https://travis-ci.org/drj11/sixtyforth.svg?branch=master)](https://travis-ci.org/drj11/sixtyforth)

Welcome to SixtyForth, `64th` for short.

SixtyForth is a 64-bit implementation of FORTH
written in 64-bit Intel assembler.

SixtyForth implements the ANSI Forth standard (1994 edition).
The CORE word set is present.

In this implementation a cell is 64-bits wide,
and signed integers are represented in 2's complement.

As per the ANSI standard,
the required words from the standard are required to be in UPPER CASE.
Obviously lower case is a bit more comfortable,
and SixtyForth may accept that in the future.
Note that you can define your own words in any case you like;
when using them the case must be exactly as you defined it.

Input is accepted from the keyboard
(and interactively ^A ^E ^P ^N ^D ^K work);
from the command line (`./64th -c 'SOURCE TYPE'`);
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

