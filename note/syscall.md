# Fun with syscall

## fstat

Typical call to `fstat()` from sixtyforth:

```
here 200 alloc 4 over 0 5 syscall3
```

Typical dump:

01 FC 00 00 00 00 00 00
49 22 80 00 00 00 00 00 st_ino
01 00 00 00 00 00 00 00 st_nlink
A4 81 00 00 E8 03 00 00 st_mode, st_uid
E8 03 00 00 00 00 00 00 st_gid
00 00 00 00 00 00 00 00
0C 00 00 00 00 00 00 00 st_size
00 10 00 00 00 00 00 00 st_blksize
08 00 00 00 00 00 00 00 st_blocks
33 BD A0 58 00 00 00 00 atime?
05 29 68 0F 00 00 00 00
38 C4 A0 58 00 00 00 00 mtime?
86 BA BD 0A 00 00 00 00
38 C4 A0 58 00 00 00 00 ctime?
86 BA BD 0A 00 00 00 00
00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 37 37 37 37 37 37

The '37' is the junk used to fill the buffer.

The structure size (as written) is 144 bytes.

st_size is at offset 48.
