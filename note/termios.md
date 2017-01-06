# How to test isatty

From

https://opensource.apple.com/source/Libc/Libc-167/gen.subproj/termios.c

it seems that we can do

    ioctl(fd, TIOCGETA, t)

What we actually do is

    ioctl(fd, TCGETS, p)

return value is 0 for success, -1 for error.

TCGETS is 0x5401 according to `/usr/include/asm-generic/ioctls.h`.

We have to be careful with the pointer types.
in (the library function) `tcsetattr`,
the pointer is a pointer to `struct termios`.

However, in the `ioctl` system call,
the pointer is a pointer to `struct __kernel_termios`.

## Empirical evidence

`ioctl(fd, 0x5401, p)` overwrites 36 bytes of memory.
Which I guess (from `tcgetattr` man page)
is 4 4-byte words and 20 1-byte characters.

## Structure conversion

The documentated termios interfaces
(for example `tcgetattr` and `tcsetattr`)
use `struct termios`.
But they are actually wrappers around the ioctl syscall.
Which uses a different structure `struct __kernel_termios`.
(the structures differ in a couple of fields
and the length of cc fields).

So the ABI (which can't really change) is based around
`struct __kernel_termios`.
Which is not really documented.

found definition of struct termios in
/usr/include/x86_64-linux-gnu/bits/termios.h

Then I decided it was foolish
to determine how big this structure is.
Just use a buffer that's obviously big enough.
98 bytes should do.

What we do is use the dictionary free space.

Then I wrote a C program to print out the size
(of struct termios).
It is 60 bytes.
