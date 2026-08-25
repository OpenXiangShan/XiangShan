    # Name   : fe_ifu_predchecker_correct_branch_widths
    # Goal   : train taken RVI and RVC conditional branches and observe both
    #          widths at real PredChecker input/output.

    .section .text, "ax"
    .option rvc
    .globl _start

_start:
    addi s0, zero, 32
    .balign 64

rvi_loop:
    addi s0, s0, -1
    bnez s0, rvi_loop

    c.li s0, 31

rvc_loop:
    c.addi s0, -1
    c.bnez s0, rvc_loop

done:
    .option push
    .option norvc
    addi a0, zero, 0
    .word 0x0005006b
    .option pop

    .rept 128
    c.nop
    .endr
