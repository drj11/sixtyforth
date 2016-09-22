# SixtyForth

Welcome to SixtyForth, `64th` for short.

SixtyForth a 64-bit implementation of FORTH
written in 64-bit Intel assembler.

The SixtyForth language is inspired by the FORTH-83 standard,
but since it does not follow the standard exactly,
it should probably be more accurately described as
a FORTH-like language.

The major departure is that a cell is not 16 bits wide,
but 64 bits wide.
A further departure is that _counted strings_
do not being with
a byte length, but with
a 64-bit length.
`count` is suitably modified for this convention.

Input is mostly assumed to be in lower case.
Due to a whimsical quirk and
a fondness for 1980s style documentation,
various documents will use upper case
to refer to FORTH words.
For example, `CREATE`.
But generally these should be
typed in lower case
in order to be
recognised by the SixtyForth system.

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

