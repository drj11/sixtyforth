Blog about what a threaded language is

Blog about D+-

Blog about using CREATE DOES> to make 1+ 1- 2+ 2-

Blog about implementing DOES>

Blog about bootstrapping from really bad languages: better
notation (for example, before negative numbers were supported in
input you had to use `97 negate` to get them; before doubles
are allowed in input you have to input them as two singles;
before D. was implemented you had to output doubles as two
singles)

Blog about how the building blocks
that the standards provide
are frustrating.
For example,
`#` requires that there is
a method of dividing a double-cell number
by a small positive number giving
a double-cell quotient
(and implicitly a remainder which
is transformed into the next digit).
`M*/` might be useful in this, but unfortunately
`#` is required to work with unsigned double-cell numbers
and `M*/` only works with
signed double-cell numbers.
In the case of SixtyForth I implemented `UM*/MOD`.

Blog about the philosophy of Forth.
