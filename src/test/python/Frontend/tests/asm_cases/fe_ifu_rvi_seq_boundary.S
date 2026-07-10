    # Name   : fe_ifu_rvi_seq_boundary
    # Goal   : directed IFU RVI boundary/predecode coverage on normal icache path
    # Covers : RVI 4B PC step, RVI high-half suppression, non-CFI decode,
    #          fetch-block head/mid/tail valid positions

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi s0, zero, 0
    addi s1, zero, 1
    addi s2, zero, 2

    .rept 5
    nop
    .endr
mid_rvi_region:
    addi s0, s0, 1
    addi s1, s1, 1
    addi s2, s2, 1
    addi s3, zero, 3

    .rept 3
    nop
    .endr
tail_rvi_region:
    addi s4, zero, 4

next_block_head:
    addi s5, zero, 5
    addi s6, s6, 1

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 128
    nop
    .endr
