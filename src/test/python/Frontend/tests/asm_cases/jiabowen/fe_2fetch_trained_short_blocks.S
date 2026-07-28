    # Name   : fe_2fetch_trained_short_blocks
    # Goal   : train a chain of short taken blocks so FTQ can issue two fetches

    .section .text, "ax"
    .option rvc
    .globl _start

_start:
    addi s0, zero, 32

    .balign 32
loop_head:
    .rept 15
    c.nop
    .endr
    c.j block1

block1:
    .rept 15
    c.nop
    .endr
    c.j block2

block2:
    .rept 15
    c.nop
    .endr
    c.j block3

block3:
    .rept 15
    c.nop
    .endr
    c.j block4

block4:
    .rept 15
    c.nop
    .endr
    c.j block5

block5:
    .rept 15
    c.nop
    .endr
    c.j block6

block6:
    .rept 15
    c.nop
    .endr
    c.j block7

block7:
    .rept 13
    c.nop
    .endr
    c.addi s0, -1
    c.bnez s0, loop_head

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 128
    c.nop
    .endr
