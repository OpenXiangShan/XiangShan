    # Name   : fe_multi_branch_random_positions
    # Source : test_multi_branch_random_positions
    # Goal   : deterministic stride-spaced mix of direct jumps and not-taken branches

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
slot0:
    jal zero, slot1
    nop

slot1:
    bne zero, zero, slot2
    addi s0, s0, 1

slot2:
    jal zero, slot3
    nop

slot3:
    bne zero, zero, slot4
    addi s1, s1, 1

slot4:
    jal zero, tail
    nop

tail:
    addi s0, zero, 1
    nop
    nop
    nop

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 16
    nop
    .endr
