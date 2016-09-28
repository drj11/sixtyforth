# A Forth-83 ticklist

KEY

i - implemented
m - implemented in a modified fashion
p - partially implemented (for example, small inputs only)


## Nucleus layer 


!       i
*       i
*/
*/MOD
+       i
+!      i
-       i
/       p (check rounding mode)
/MOD
0<      i
0=      i
0>      i
1+
1-      i
2+
2-
2/
<       i
=       i
>
>R
?DUP    i
@       i
ABS     i
AND
C!      i
C@      i
CMOVE   i
CMOVE>
COUNT   m
D+
D<
DEPTH   i
DNEGATE
DROP    i
DUP     i
EXECUTE i
EXIT    i
FILL
I
J
MAX
MIN
MOD
NEGATE  i
NOT
OR      i
OVER    i
PICK
R>
R@
ROLL
ROT     i
SWAP    i
U<
UM*
UM/MOD  i
XOR     i


## Device layer 


BLOCK
BUFFER
CR
EMIT
EXPECT
FLUSH
KEY
SAVE-BUFFERS
SPACE
SPACES
TYPE    i
UPDATE


## Interpreter layer 


#       i
#>      i
#S      i
#TIB    i
'       i
(
-TRAILING
.       i
.(
<#      i
>BODY   i
>IN     i
ABORT
BASE    i
BLK
CONVERT
DECIMAL
DEFINITIONS
FIND    i
FORGET
FORTH
FORTH-83
HERE    i
HOLD    i
LOAD
PAD
QUIT    i
SIGN    i
SPAN
TIB     i
U.      i
WORD    i


## Compiler layer 


+LOOP
,       i
."
:       i
;       i
ABORT"
ALLOT   i
BEGIN
COMPILE
CONSTANT
CREATE  i
DO
DOES>
ELSE
IF
IMMEDIATE
LEAVE
LITERAL i
LOOP
REPEAT
STATE   i
THEN
UNTIL
VARIABLE
VOCABULARY
WHILE
[
[']
[COMPILE]
]       i
