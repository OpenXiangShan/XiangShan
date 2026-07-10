    # Name   : fe_baremode_cond_nt
    # Source : test_baremode_cond_nt_coverage_pilot
    # Goal   : conditional branch decoded but not taken on the normal icache path

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi s0, zero, 0
    addi s1, zero, 1

cond_nt_site:
    bne zero, zero, cond_nt_target
    addi s0, s0, 1
    addi s1, s1, 1

cond_nt_target:
    nop
    nop
    nop
    nop

done:
    addi a0, zero, 0
    .word 0x0005006b
