    # Name   : fe_2fetch_size_blocked
    # Goal   : make two same-page candidates exceed the 32-halfword 2-fetch limit

    .section .text, "ax"
    .option rvc
    .globl _start

_start:
    addi s0, zero, 32
    j long_block

    # The current DUT's fall-through block is 64B/32 halfwords.  With no CFI
    # it becomes the first candidate; the following 16-halfword block makes
    # their total length exceed the FTQ two-fetch limit.
    .balign 64
long_block:
    .rept 32
    c.nop
    .endr

block1:
    .rept 15
    c.nop
    .endr
    c.j block2

    .balign 32
block2:
    .rept 15
    c.nop
    .endr
    c.j block3

    .balign 32
block3:
    .rept 15
    c.nop
    .endr
    c.j block4

    .balign 32
block4:
    .rept 15
    c.nop
    .endr
    c.j loop_tail

    .balign 32
loop_tail:
    .rept 13
    c.nop
    .endr
    c.addi s0, -1
    c.bnez s0, long_block
    c.nop

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 128
    c.nop
    .endr
