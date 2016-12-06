# How to test isatty

From

https://opensource.apple.com/source/Libc/Libc-167/gen.subproj/termios.c

it seems that we can do

    ioctl(fd, TIOCGETA, t)

return value is 0 for success, -1 for error;
`t` is a pointer to `struct termios`.

What we actually do is

    ioctl(fd, TCGETS, p)

TCGETS is 0x5401 according to `/usr/include/asm-generic/ioctls.h`.

found definition of struct termios in
/usr/include/x86_64-linux-gnu/bits/termios.h

Then I decided it was foolish
to determine how big this structure is.
Just use a buffer that's obviously big enough.
98 bytes should do.

What we do is use the dictionary free space.

Then I wrote a C program to print out the size.
It is 60 bytes.
