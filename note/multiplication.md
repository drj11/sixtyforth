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
This works because signed multiplication can be used
to multiply unsigned numbers as long as the numbers are
not greater than `MAX-N`;
for unsigned numbers larger than `MAX-N`,
only one operand can be greater than `MAX-N`,
the other must be 0 or 1.
Signed multiply works in that case too.
