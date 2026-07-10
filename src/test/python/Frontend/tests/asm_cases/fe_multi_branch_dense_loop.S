    # Name   : fe_multi_branch_dense_loop
    # Source : test_multi_branch_dense_loop
    # Goal   : dense branch decode stream with a forward jal tail

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
loop_head:
    bne zero, zero, dense_b1
    addi s0, s0, 1
dense_b1:
    bne zero, zero, dense_b2
    addi s1, s1, 1
dense_b2:
    bne zero, zero, dense_tail
    addi s2, s2, 1

dense_tail:
    jal zero, done

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 16
    nop
    .endr
