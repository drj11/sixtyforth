# Implementing +LOOP

## Definitions

INC the loop increment

I index before increment

I' index after increment

L the loop limit

measure `I L -` before and after increment.
If of opposite sign then terminate loop.

problem: example: INC = 1, L = 0, I=2**63-1

I and I' have opposite sign, but are nowhere near limit

So we must consider sign of INC too.
Altogether:

INC     I-L     I'-L    TERMINATE?
+       -       +       Yes
+       +       -       No
-       -       +       No
-       +       -       Yes

I I' XOR  INC I' XOR INVERT  AND

gives a result that has sign bit set (is negative)
when loop terimination condition is met.
