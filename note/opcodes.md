# A note on opcodes

Let us consider a dump of the PLUS definition:

```
' + 96 cr dump
56 13 60 00 00 00 00 00 48 8B 45 F0 48 8B 4D F8 48 01 C8 48 83
ED 08 48 89 45 F8 E9 CE ED DF FF 36 13 60 00 00 00 00 00 01 00
00 00 00 00 00 00 2D 00 00 00 00 00 00 00 8E 13 60 00 00 00 00
00 48 8B 45 F0 48 8B 4D F8 48 29 C8 48 83 ED 08 48 89 45 F8 E9
96 ED DF FF 6E 13 60 00 00 00 00 00  ok
```

Let's break that down:

```
56 13 60 00 00 00 00 00         Address of next address. $+8
48                              REX.W
8B                              MOV
45 F0                           RAX, [RBP   -16]
48                              REX.W
8B                              MOV
4D F8                           RCX, [RBP   -8]
48                              REX.W
01                              ADD
C8                              RAX, RCX
48 83                           REX.W ArithOP Immediate
ED 08                           SUB RBP, 8
                                Note above, SUB is encoded in 3
                                bits of ModR/M byte, as well as
                                opcode byte.
48                              REX.W
89                              MOV
45 F8                           [RBP-8], RAX
E9                              JMP
CE ED DF FF                     Signed displacement
```

The `REX.W` prefix is used to access 64-bit registers
(both the 64-bit version of the 32-bit registers and
the 64-bit only register R8 thru R15).
Because we are generally writing 64-bit code,
this appears a lot.

In an instruction like `MOV`, 8B 45 F0,
8B is the instruction opcode,
45 is the ModR/M byte that specifies both source and destination,
and F0 is an 8-bit signed displaced added
because the Mod bits in the ModR/M byte are 01.

The ModR/M byte encodes a field that is either
a register or a memory location (R/M),
and another field that is a register (/r64 for 64-bit instructions).
Which is these is the destination varies from opcode to opcode.
Note for `MOV` 8B /r64 is the destination,
for `ADD` 01 R/M is the destination.
There is an alternate encoding for `ADD`, 03, that makes /r64
the destination.
