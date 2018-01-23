# The Untested

Some words are problematic to test.

`ABORT`.
Appears to work, but is difficult to test because in
non-interative `EVALUATE`s (such as `-c`),
`ABORT` consumes the entire remaining input.
