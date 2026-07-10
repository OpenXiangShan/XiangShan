    # Name   : fe_baremode_direct_jmp
    # Source : test_baremode_direct_jmp_coverage_pilot
    # Goal   : direct jal on the normal icache path

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi s0, zero, 0
    addi s1, zero, 1

direct_jmp_site:
    jal zero, direct_jmp_target
    addi s0, s0, -1
    addi s0, s0, -1

direct_jmp_target:
    addi s0, s0, 1
    nop
    nop
    nop
    nop

done:
    addi a0, zero, 0
    .word 0x0005006b
