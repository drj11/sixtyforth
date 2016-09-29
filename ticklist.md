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
1+      i
1-      i
2+      i
2-      i
2/
<       i
=       i
>       i
>R      i
?DUP    i
@       i
ABS     i
AND     i
C!      i
C@      i
CMOVE   i
CMOVE>
COUNT   m
D+      i
D<
DEPTH   i
DNEGATE i
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
R>      i
R@      i
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
IMMEDIATE       i
LEAVE
LITERAL i
LOOP
REPEAT
STATE   i
THEN
UNTIL
VARIABLE        i
VOCABULARY
WHILE
[
[']
[COMPILE]
]       i
