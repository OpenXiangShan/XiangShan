    # Name   : fe_jal_forward_jump_observes_target_pc
    # Source : test_jal_forward_jump_observes_target_pc
    # Goal   : forward jal target is observed in the bin-trace path

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    jal zero, target_pc
    addi s0, s0, -1

target_pc:
    addi s0, zero, 1
    nop
    nop
    nop

done:
    addi a0, zero, 0
    .word 0x0005006b
