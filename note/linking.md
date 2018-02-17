# Linking Source Files

The current build function is:

Asm(*.asm) → *.o

Objcopy(*.4) → *.o

Link(*.o) → 64th

The standard build has only one `asm` file, `64th.asm`;
and only one `.4` Forth file, `rc.4`.

The definition of the threaded code word `RC` in `64th.asm` ties
the implemention to the `rc.4` file.
Though the design is in principle flexible enough to incorporate
other Forth files.

This allows us to create a single static executable
with parts written in assembler and threaded code (`.asm` files)
and parts written in Forth (`.4` files).


## Internal execution

A small Forth initialisation routine can then
evaluate this string.

The answer to my stack overflow question explains
how to convert a file into an object file (by literal inclusion):

See http://stackoverflow.com/questions/42235175/how-do-i-add-contents-of-text-file-as-a-section-in-an-elf-file

Create an object file from a raw "binary" file:

    objcopy --input binary --output elf64-x86-64 --binary-architecture i386:x86-64 rc.4 rc.o

Create `rc.o` from the input binary `rc.4`
(which in our case is text, but `objcopy` doesn't care,
it just uses the contents directly).

The object file comes with symbols that describe its bounds.
Use `objdump -x rc.o` to see these:

```
_binary_rc_4_start
_binary_rc_4_end
_binary_rc_4_size
```

These can be used from assembly by using the NASM `extern` directive.
See the implementation of `RC` in `64th.asm`.

From a running SixtyForth,
we can evaluate the (internal version of the) file `rc.4` with:

    rc.4 EVALUATE

(or we could if `rc.4` was defined in the dictionary).
The internal version of this in threaded code is implemented.

