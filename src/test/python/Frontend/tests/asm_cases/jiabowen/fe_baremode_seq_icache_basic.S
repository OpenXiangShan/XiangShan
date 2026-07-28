    # Name   : fe_baremode_seq_icache_basic
    # Source : test_baremode_seq_icache_basic_pilot
    # Goal   : straight-line baremode instruction stream on the normal icache path

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi s0, zero, 0
    addi s1, zero, 1
    addi s2, zero, 2
    addi s3, zero, 3

    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop

    addi s0, s0, 1
    addi s1, s1, 1
    addi s2, s2, 1
    addi s3, s3, 1

done:
    addi a0, zero, 0
    .word 0x0005006b
