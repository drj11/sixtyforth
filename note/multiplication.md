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

## `fm/mod` or `sm/rem` as a primitive.

The Standard expects that
either `sm/rem` or `fm/mod` will be a primitive,
and that other operators can be defined
in terms of that primitive.

The Standard commentary provides a reasonable strategy:

Define `/mod` and `*/mod`:

`: /mod >r s>d r> sm/rem ;`

`: */mod >r m* r> sm/rem ;`

Note the similarity, both produce a double
(either via `s>d` or `m*`), then invoke `sm/rem`.

Then `/` and `*/` just drop the remainder parts:

`: / /mod nip`

`: */ */mod nip`.

## Primitives on Intel-64

`sm/rem` is primitive.
Corresponds to `idiv`

`um/mod` is primitive.
Corresponds to `div`.

`m*` is primitive.
Corresponds to `imul`.

`um*` is primitive.
Corresponds to `mul`.

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

(Historical note: there was a TIL version of this
in `64th.asm` called `ud/mod`,
but we have replaced it with the above definition in Forth.)

Note that I has to invent `u/mod`;
it's `: u/mod 0 swap um/mod ;`.

The first division leaves `L Mrem` on the stack.
In long division,
the remainder of one division forms the most significant part
of the dividend that enters the next round of long division.
`L Mrem` is, conveniently,
the 2 cells that make up the double cell dividend.

## SixtyForth

The thrust of this proposal is the following observations:

- `>number` requires double × single → double multiplication;
- `#` requires double × single → double division;
- both of those operations are unusual (not standard);
- `#` is not required in the ASM core;
- may be able to reduce functionality of `>number` in ASM core.

'#' and related machinery have been moved out of ASM core
(and implemented in Forth).

There remains the possibility of moving
`>number` out of the ASM core and into Forth code.
And in doing so, move a bunch of
annoying long division and multiplication out of ASM.

## Dependencies


. u. d. -> <# #s #>

Interpreter `qNUMBER` word -> `>number`

>number -> ud*

`>number` is really horrifically large.
Good idea to factor it, and/or implement in Forth.

* -> m*

m*/ -> um*/mod
ud* -> um*/mod

um*/mod 64th primitive

um*/mod used only by m*/ and ud*

ud* used solely by >number

ud* should be written in terms of `um*` and `d+`
(rather than this weirdly huge `um*/mod`).

: ud* ( ud u -- ud )
   ( ul um u )
   swap over    ( ul u um u )
   *            ( ul u mprod )
   >r           ( ul u )
   um*          ( ud )
   0 r>         ( ud 0 mprod )
   d+           ( udprod )
;

m*/ not used

um/mod ANSI primitive

m* ANSI primitive
um* ANSI primitive

sm/rem ANSI primitive

`#` and following moved into Forth

# -> uml/mod

uml/mod -> um/mod

uml/mod solely used by #
