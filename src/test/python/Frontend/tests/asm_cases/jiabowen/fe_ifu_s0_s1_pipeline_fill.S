    # Name   : fe_ifu_s0_s1_pipeline_fill
    # Goal   : repeatedly fill IFU s1 from accepted cacheable ICache returns

    .section .text, "ax"
    .option rvc
    .globl _start

_start:
    li s0, 64
    li s1, 0

    # The aligned body spans multiple cachelines and mixes RVC with RVI.  Each
    # iteration returns to the same cacheable path so s0->s1 pipeline fills are
    # sampled both before and after the loop branch becomes trained.
    .balign 64
pipeline_fill_loop:
    .rept 20
    c.nop
    .endr
    addi s1, s1, 1
    .rept 10
    c.nop
    .endr
    c.addi s0, -1
    c.bnez s0, pipeline_fill_loop

done:
    li a0, 0
    .word 0x0005006b
    .rept 64
    c.nop
    .endr
