# A Forth-83 ticklist

KEY

i - implemented
u - implemented but untested
p - partially implemented (for example, small inputs only)


## Nucleus layer 


!       i
*       i
*/      i
*/MOD   p
+       i
+!      i
-       i
/       i
/MOD
0<      i
0=      i
0>      i
1+      i
1-      i
2+      i
2-      i
2/      i
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
CMOVE>  i
COUNT   u
D+      i
D<
DEPTH   i
DNEGATE i
DROP    i
DUP     i
EXECUTE i
EXIT    i
FILL    i
I       i
J       i
MAX     i
MIN     i
MOD     i
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
U<      i
UM*     i
UM/MOD  i
XOR     i


## Device layer 


BLOCK
BUFFER
CR      i
EMIT    i
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
#TIB    x
'       i
(       i
-TRAILING
.       i
.(      i
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
TIB     x
U.      i
WORD    x


## Compiler layer 


+LOOP   i
,       i
."      i
:       i
;       i
ABORT"  i
ALLOT   i
BEGIN   i
COMPILE p
CONSTANT        i
CREATE  i
DO      i
DOES>   i
ELSE    i
IF      i
IMMEDIATE       i
LEAVE   i
LITERAL i
LOOP    i
REPEAT  i
STATE   i
THEN    i
UNTIL   i
VARIABLE        i
VOCABULARY
WHILE   i
[       i
[']     i
[COMPILE]       i
]       i

## From std1994

<>      i
>NUMBER i
?DO     i
/STRING i
2!      i
2*      i
2@      i
2DROP   i
2DUP    i
2OVER   i
2ROT    i
2SWAP   i
ALIGNED i
BL      i
C,      i
CELL+   i
CELLS   i
CHAR    i
DECIMAL u
D.      i
D>S     i
DABS    i
EVALUATE        i
FALSE   i
HEX     i
INVERT  i
M*      i
M*/     i
NIP     i
PARSE   i
RECURSE i
S"      i
S>D     i
SM/REM  i
TO      i
TRUE    i
TUCK    i
U>      i
VALUE   i
WITHIN  i
[CHAR]  i
