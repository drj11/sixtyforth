# SixtyForth

[![Build Status](https://travis-ci.org/drj11/sixtyforth.svg?branch=master)](https://travis-ci.org/drj11/sixtyforth)

Welcome to SixtyForth, `64th` for short.

SixtyForth a 64-bit implementation of FORTH
written in 64-bit Intel assembler.

The SixtyForth language is
inspired by the ANSI Forth standard (1994 edition),
but since it does not follow the standard exactly,
it should probably be more accurately described as
a Forth-like language.

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

## Bugs, Issues, Fun

Please refer to github.

https://github.com/drj11/sixtyforth/

