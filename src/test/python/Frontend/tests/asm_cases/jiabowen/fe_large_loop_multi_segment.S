    # Name   : fe_large_loop_multi_segment
    # Source : test_large_loop_multi_segment
    # Goal   : large multi-segment stream with mixed taken/not-taken branches

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
segment0:
    bne zero, zero, segment0_after_beq
    addi s0, s0, 1
segment0_after_beq:
    bne zero, zero, segment0_after_bne
    addi s0, s0, 1
segment0_after_bne:
    .rept 250
    nop
    .endr
    jal zero, segment1

segment1:
    bne zero, zero, segment1_after_beq
    addi s1, s1, 1
segment1_after_beq:
    bne zero, zero, segment1_after_bne
    addi s1, s1, 1
segment1_after_bne:
    .rept 250
    nop
    .endr
    jal zero, segment2

segment2:
    bne zero, zero, segment2_after_beq
    addi s2, s2, 1
segment2_after_beq:
    bne zero, zero, segment2_after_bne
    addi s2, s2, 1
segment2_after_bne:
    .rept 250
    nop
    .endr
    jal zero, segment3

segment3:
    bne zero, zero, segment3_after_beq
    addi s3, s3, 1
segment3_after_beq:
    bne zero, zero, segment3_after_bne
    addi s3, s3, 1
segment3_after_bne:
    .rept 250
    nop
    .endr

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 16
    nop
    .endr
