# Multipicative operators in Forth and Sixtyforth

From ANSI [std 1994]

*       ( w1 w2 -- w3 )

*/      ( n1 n2 n3 -- n4 )

*/MOD   ( n1 n2 n3 -- rem quot )

/       ( n1 n2 -- n3 )

/MOD    ( n1 n2 -- rem quot )

M*      ( n1 n2 -- d )

UM*     ( u1 u2 -- ud )

UM/MOD  ( ud u1 -- rem quot )

M*/     ( d1 n1 +n2 -- d2 )                             \ DOUBLE

The division in the above operators is ambiguous
with respect to rounding direction.
The standard provides `FM/MOD` and `SM/REM` that have
unambiguous rounding directions
(_floor_ and _symmetric_ respectively).

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

`: * M* DROP`

## `FM/MOD` or `SM/REM` as a primitive.

The Standard expects that
either `SM/REM` or `FM/MOD` will be a primitive,
and that other operators can be defined
in terms of that primitive.

The Standard commentary provides a reasonable strategy:

Define `/MOD` and `*/MOD`:

`: /MOD >R S>D R> SM/REM ;`

`: */MOD >R M* R> SM/REM ;`

Note the similarity, both produce a double
(either via `S>D` or `M*`), then invoke `SM/REM`.

Then `/` and `*/` just drop the remainder parts:

`: / /MOD NIP`

`: */ */MOD NIP`.

## Primitives on Intel-64

`SM/REM` is primitive.
Corresponds to `idiv`

`UM/MOD` is primitive.
Corresponds to `div`.

`M*` is primitive.
Corresponds to `imul`.

`UM*` is primitive.
Corresponds to `mul`.

## The problem of #

`#` ( ud1 -- ud2 ) takes and returns a (unsigned) double.
None of the previously discussed division words do that.
Making it a little problematic to implement `#`.

I implement `#` using a word called `uml/mod`,
implemented in Forth.


```
`: uml/mod ( ud u -- u-rem ud-quot )`
\ We can use long division.
( ud u ) \ is same as ( L M u )
>r r@
u/mod  ( L Mrem Mquot )
\ observe L Mrem is the (double) input in next round of long division,
\ Mquot is the M part of the ud-quot
r> swap >r      ( L Mrem u )
um/mod          ( Lrem Lquot )
r>              ( Lrem Lquot Mquot )
                ( u-rem ud-quot )
;
```

The factor `u/mod` is defined: `: u/mod 0 swap um/mod ;`.

The first division leaves `L Mrem` on the stack.
In long division,
the remainder of one division forms the most significant part
of the dividend that enters the next round of long division.
`L Mrem` is, conveniently,
the 2 cells that make up the double cell dividend.

## SixtyForth

The thrust of this proposal is the following observations:

- `>NUMBER` requires double × single → double multiplication;
- `#` requires double × single → double division;
- both of those operations are unusual (not standard);
- `#` is not required in the ASM core;
- may be able to reduce functionality of `>NUMBER` in ASM core.

'#' and related machinery have been moved out of ASM core
(and implemented in Forth).

There remains the possibility of moving
`>NUMBER` out of the ASM core and into Forth code.
And in doing so, move a bunch of
annoying long division and multiplication out of ASM.

## Dependencies


. U. D. -> <# #s #>

`qNUMBER` (the interpreter internal) -> `>NUMBER`

`>NUMBER` -> `ud*` (sole use)

`>NUMBER` is really horrifically large.
Good idea to factor it, and/or implement in Forth.

`*` -> `M*`

`ud*` -> `UM*` `D+`

`ud*` is written in terms of `UM*` and `D+` (in threaded code):

    : ud* ( ud u -- ud )
       ( ul um u )
       SWAP OVER        ( ul u um u )
       * >R             ( ul u )        ( r: mprod )
       UM*              ( ud )
       0 R>             ( ud 0 mprod )  ( r: )
       D+               ( udprod )
    ;

`UM/MOD` ANSI primitive

`M*` ANSI primitive
`UM*` ANSI primitive

`SM/REM` ANSI primitive

`#` and following moved into Forth

`#` -> `uml/mod` (sole use)

`uml/mod` -> `UM/MOD`
