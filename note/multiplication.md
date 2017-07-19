# Multipicative operators in Forth and Sixtyforth

From ANSI [std 1994]

*       ( w1 w2 -- w3 )

*/      ( n1 n2 n3 -- n4 )

*/mod   ( n1 n2 n3 -- rem quot )

/       ( n1 n2 -- n3 )

/mod    ( n1 n2 -- rem quot )

m*      ( n1 n2 -- d )

um*     ( u1 u2 -- ud )

um/mod  ( ud u1 -- rem quot )

From ANSI DOUBLE extension

m*/     ( d1 n1 +n2 -- d2 )

# commentary

Note that `*` (which is single × single → single)
can be used for either signed or unsigned integers.
Signed multiplication can be used regardless.
This works because
the results are only defined when the result fits in a single cell.
The operation is only problematic if one of the inputs
is an unsigned integer larger than `MAX-N`.
In this case, if the result is to fit in a single cell,
hence be defined,
the other input must be 0 or 1
(any number larger than 1 will yield a mathemetical result
that is out of the defined range).
Signed multiply works in that case too.
