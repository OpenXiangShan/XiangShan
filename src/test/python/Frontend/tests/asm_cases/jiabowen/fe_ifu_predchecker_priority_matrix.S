    # Name   : fe_ifu_predchecker_priority_matrix
    # Goal   : keep an early not-taken JALR and a younger JAL in one checker request

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    .balign 64
    # The first indirect jump is deliberately left cold.  Its target is the
    # next aligned block, while the younger direct jump remains in this fetch
    # block so V3 must select the earliest JALR fault.
    auipc t1, 0
    addi t1, t1, 64
    jalr s1, 0(t1)
    jal zero, target
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop

    .balign 64
target:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 128
    nop
    .endr
