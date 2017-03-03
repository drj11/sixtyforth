# Readline

(It's now called Key Line in the code,
using a `ki` prefix)

By running `stty` from another terminal
we can inspect how bash/readline uses the various tty modes.

Compared to normal terminal operation, readline:

- switches off `icanon` (0x2 in c_lflag, word 3)
- switches off `echo` (0x8 in c_lflag)
- switches off `icrnl` (0x100 in c_iflag, word 0)

(we will have to do something similar)

The effect of these is:
- to return input characters as soon as the are available
  (assuming min=1, which it is by default);
- to avoid automatically echoing input characters
  (leaving readline to do all echoing explicitly);
- (`icrnl`) avoids translating CR to NL (presumably so that
  readline can handle CR under its own control).

# Updating the line

When text is inserted into the line,
the line from the inserted material to the end of the line
is damaged,
and needs reprinting.

Whenever a damaged part needs reprinting,
the cursor needs moving from its current position
to the beginning of the damaged region.
The ANSI escape ESC [ J may be useful (clear to end of screan).

Whenever the line is reprinted,
the cursor needs moving from its current position
to its target position.

Note: often the reprinted material will cover
the (new) target cursor position.
In which case, SCP and RCP (`CSI s` and `CSI u`)
could be used to save and restore the cursor position.
Though they may be non-standard
(I think it's in the private use area of ECMA-48).
But they appear to work on Gnome Terminal.

# Buffer state

- ki.a address of beginning of buffer
- ki.z size of buffer
- ki.> position of cursor
- ki.n number of valid input characters

## Further Reading

https://github.com/antirez/linenoise/ is fun
