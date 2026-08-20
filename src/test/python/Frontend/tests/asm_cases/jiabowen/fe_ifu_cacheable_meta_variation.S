    # Name   : fe_ifu_cacheable_meta_variation
    # Goal   : exercise cacheable IFU block position, alignment, range, and FTQ metadata diversity

    .section .text, "ax"
    .option rvc
    .globl _start

_start:
    li s0, 32

    # Each cacheline visits byte offsets 0, 2, 32, and 48.  The short taken
    # blocks train both 2B-only and 4B-aligned target entries while repeatedly
    # crossing into the next cacheline.
    .balign 64
line_head:
    c.j line_head_2b
line_head_2b:
    .rept 14
    c.nop
    .endr
    c.j line_mid
line_mid:
    .rept 7
    c.nop
    .endr
    c.j line_tail
line_tail:
    .rept 6
    c.nop
    .endr
    c.addi s0, -1
    c.bnez s0, line_head

done:
    li a0, 0
    .word 0x0005006b
    .rept 64
    c.nop
    .endr
