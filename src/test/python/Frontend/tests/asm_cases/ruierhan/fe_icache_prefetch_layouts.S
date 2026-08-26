    # Name   : fe_icache_prefetch_layouts
    # Source : test_tc_icache_prefetchpipe_refill_layout
    # Goal   : train short fetch blocks at several 64-byte-relative branch sites

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi t0, zero, 32

    .balign 64
same_line_loop:
    .rept 3
    nop
    .endr
    beq zero, zero, overlap1_loop
    .rept 10
    nop
    .endr

    .balign 64
overlap1_loop:
    .rept 11
    nop
    .endr
    beq zero, zero, interleave_loop
    .rept 2
    nop
    .endr

    .balign 64
interleave_loop:
    addi t0, t0, -1
    bne t0, zero, same_line_loop
    .rept 13
    nop
    .endr

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 16
    nop
    .endr
