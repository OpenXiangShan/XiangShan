    # Name   : fe_jal_resolve_drains_pending_queue
    # Source : test_jal_resolve_drains_pending_queue
    # Goal   : forward jal after warmup nops

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    .rept 8
    nop
    .endr

jal_site:
    jal zero, jal_target
    addi s0, s0, -1

jal_target:
    addi s0, zero, 1
    nop
    nop
    nop

done:
    addi a0, zero, 0
    .word 0x0005006b
