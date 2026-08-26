    # Name   : fe_icache_prefetch_bpu_flush
    # Source : test_tc_icache_prefetchpipe_bpu_flush
    # Goal   : repeatedly retrain one conditional branch and a taken block jump

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi t0, zero, 64

    .balign 64
train_loop:
    andi t1, t0, 1
    beq t1, zero, even_path
    addi s0, s0, 1
    jal zero, loop_tail

even_path:
    addi s1, s1, 1
    .rept 5
    nop
    .endr

loop_tail:
    addi t0, t0, -1
    bne t0, zero, train_loop

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 16
    nop
    .endr
