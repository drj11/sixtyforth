# Linking Source Files

We would like to write Forth in plain text files.

Including the Forth core that gets automatically evaluated.

drj had this idea to incorporate Forth source files
(plain text files, basically) into the executable
at executable construction time,
in such a way that the source appears somewhere in the memory map.

A small Forth initialisation routine can then
evaluate this string.

The story so far:

See http://stackoverflow.com/questions/42235175/how-do-i-add-contents-of-text-file-as-a-section-in-an-elf-file

Create an object file from a raw "binary" file:

    objcopy --input binary --output elf64-x86-64 --binary-architecture i386:x86-64 rc.4 rc.o

Create `rc.o` from the input binary `rc.4`
(which in our case is test, but `objcopy` doesn't care,
it just uses the contents directly).

The object file comes with symbols that describe its bounds.
Use `objdump -x rc.o` to see these:

```
_binary_rc_4_start
_binary_rc_4_end
_binary_rc_4_size
```

These can be used from assembly by using the NASM `extern`
directive.
See the implementation of `rc.4` in `64th.asm`.

From a running sixtyforth, we can evaluate the
(internal version of the) file `rc.4` with:

    rc.4 evaluate

