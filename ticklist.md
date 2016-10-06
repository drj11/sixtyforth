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
ABORT   p (not directly tested)
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
ABORT"  i
ALLOT   i
BEGIN   i
COMPILE
CONSTANT
CREATE  i
DO
DOES>
ELSE    i
IF      i
IMMEDIATE       i
LEAVE
LITERAL i
LOOP
REPEAT  i
STATE   i
THEN    i
UNTIL   i
VARIABLE        i
VOCABULARY
WHILE   i
[
[']
[COMPILE]
]       i

## From std1994

<>      i
>NUMBER i
2OVER   i
2ROT    i
2SWAP   i
ALIGNED i
BL      i
CELL+   i
CELLS   i
CHAR    i
D.      i
D>S     i
DABS    i
FALSE   i
M*/     i
NIP     i
S"      i
S>D     i
TRUE    i
[CHAR]  i

PARSE
