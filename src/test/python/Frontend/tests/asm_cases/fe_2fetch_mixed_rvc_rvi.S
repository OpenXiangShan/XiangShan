    # Name   : fe_2fetch_mixed_rvc_rvi
    # Goal   : issue two short fetch blocks containing both RVC and RVI

    .section .text, "ax"
    .option rvc
    .globl _start

_start:
    addi s0, zero, 32

    .macro rvi_inc reg
    .option push
    .option norvc
    addi \reg, \reg, 1
    .option pop
    .endm

    .macro rvi_dec reg
    .option push
    .option norvc
    addi \reg, \reg, -1
    .option pop
    .endm

    # Every block occupies 16 halfwords.  The terminal compressed jump keeps
    # its predicted size at 16 halfwords, which permits two adjacent blocks.
    .balign 32
loop_head:
    .rept 5
    rvi_inc t0
    c.nop
    .endr
    c.j block1

    .balign 32
block1:
    .rept 5
    rvi_inc t1
    c.nop
    .endr
    c.j block2

    .balign 32
block2:
    .rept 5
    rvi_inc t2
    c.nop
    .endr
    c.j block3

    .balign 32
block3:
    .rept 5
    rvi_inc t3
    c.nop
    .endr
    c.j block4

    .balign 32
block4:
    .rept 5
    rvi_inc t4
    c.nop
    .endr
    c.j block5

    .balign 32
block5:
    .rept 5
    rvi_inc t5
    c.nop
    .endr
    c.j block6

    .balign 32
block6:
    .rept 5
    rvi_inc t6
    c.nop
    .endr
    c.j loop_tail

    .balign 32
loop_tail:
    .rept 4
    rvi_inc a0
    c.nop
    .endr
    rvi_dec s0
    c.nop
    c.bnez s0, loop_head

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 128
    c.nop
    .endr
