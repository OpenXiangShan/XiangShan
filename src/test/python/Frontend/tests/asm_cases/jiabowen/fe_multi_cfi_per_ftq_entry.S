    # Name   : fe_multi_cfi_per_ftq_entry
    # Source : test_multi_cfi_per_ftq_entry
    # Goal   : multiple branch CFIs in 64-byte fetch-block shaped regions

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
block0:
    bne zero, zero, block0_skip
    addi s0, s0, 1
block0_skip:
    nop
block0_b1:
    bne zero, zero, block0_tail
    addi s0, s0, 1
block0_tail:
    .rept 11
    nop
    .endr
    jal zero, block1

block1:
    bne zero, zero, block1_skip
    addi s1, s1, 1
block1_skip:
    nop
block1_b1:
    bne zero, zero, block1_tail
    addi s1, s1, 1
block1_tail:
    .rept 11
    nop
    .endr
    jal zero, block2

block2:
    addi s0, zero, 2
    nop
    nop
    nop

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 16
    nop
    .endr
