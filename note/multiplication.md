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

The division in the above operators is ambiguous
with respect to rounding direction.
The standard provides `fm/mod` and `sm/rem` that have
unambiguous rounding directions
(_floor_ and _symmetric_ respectively).

From ANSI DOUBLE extension

m*/     ( d1 n1 +n2 -- d2 )

## Polymorphism of `*`

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

Thus we can define:

`: * m* drop`

## `fm/mod` as a primitive.

The Standard commentary provides a reasonable strategy of
implementing most of these in terms of `fm/mod`
(or, structurally equivalently, `sm/rem`).

Define `/mod` and `*/mod`:

`: /mod >r s>d r> fm/mod ;`

`: */mod >r m* r> fm/mod ;`

Note the similarity, both produce a double
(either via `s>d` or `m*`), then invoke `fm/mod`.

Then `/` and `*/` just drop the remainder parts:

`: / /mod nip`

`: */ */mod nip`.

The multiplication operators,
`m*` and `um*` are, in Intel-64, convenient primitives.

Does that leave
`um/mod` as a primitive?

## The problem of #

`#` ( ud1 -- ud2 ) takes and returns a (unsigned) double.
None of the previously discussed division words do that.
Making it a little problematic to implement `#`.

Ideally, we'd have a word:

`uml/mod ( ud u -- u-rem ud-quot )`

We can use long division.

```
( ud u ) is same as ( L M u )
>r r@
u/mod  ( L Mrem Mquot )
\ observe L Mrem is the (double) input in next round of long division,
\ Mquot is the M part of the ud-quot
r> swap >r      ( L Mrem u )
um/mod          ( Lrem Lquot )
r>              ( Lrem Lquot Mquot )
                ( u-rem ud-quot )
```

Note that I has to invent `u/mod`;
it's `: u/mod 0 swap um/mod ;`.

the first division conveniently leaves `L Mrem` on the stack,
which is the double cell dividend that
enters the next round of long division.
