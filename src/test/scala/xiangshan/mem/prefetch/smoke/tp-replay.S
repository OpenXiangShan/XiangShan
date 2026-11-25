    .macro  advance_line dest, base
#ifdef TP_FULL_POOL
    add     \dest, \base, s9
#else
    addi    \dest, \base, 64
#endif
    .endm

    .option norvc
    .option nopic
    .section .text
    .globl _start
_start:
    la      t0, fail
    csrw    mtvec, t0
    la      s0, dataset
    li      s1, 16
    li      s2, 0x8e4c17a3d692f5b0
    li      s3, 4
    li      s4, 0x5a5a
    li      s5, 3
#ifdef TP_FULL_POOL
    li      s9, 0x00400040
#endif

    mv      t0, s0
    mv      t1, s1
initialize:
    sd      s4, 0(t0)
    advance_line t0, t0
    addi    t1, t1, -1
    bnez    t1, initialize
    fence   rw, rw

next_round:
    mv      t3, s2
    mv      t4, s1
load_target:
    andi    t0, t3, 15
#ifdef TP_FULL_POOL
    mul     t0, t0, s9
#else
    slli    t0, t0, 6
#endif
    add     t0, s0, t0
    ld      t1, 0(t0)
    bne     t1, s4, fail

    # The next load depends on a serial arithmetic chain without extra load PCs.
    slli    t2, t1, 1
#ifdef TP_COLD_TARGETS
    .rept   64
#else
    .rept   24
#endif
    mul     t2, t2, s5
    .endr
    andi    t2, t2, 1
    or      s0, s0, t2
    srli    t3, t3, 4
    addi    t4, t4, -1
    bnez    t4, load_target
    addi    s3, s3, -1
#ifdef TP_COLD_TARGETS
    beqz    s3, done
    li      t0, 2
    bne     s3, t0, next_round

    # Keep the trained trigger hot and evict the recorded targets before replay.
    advance_line t0, s0
    addi    t1, s1, -1
flush_targets:
    cbo.flush 0(t0)
    advance_line t0, t0
    addi    t1, t1, -1
    bnez    t1, flush_targets
    fence   rw, rw
    j       next_round
done:
#else
    bnez    s3, next_round
#endif

    li      a0, 0
    .word   0x0000006b
    j       .

    .balign 4
fail:
    li      a0, 1
    .word   0x0000006b
    j       .

    .section .bss
    .balign 4096
dataset:
#ifdef TP_FULL_POOL
    .skip   0x04000000
#else
    .skip   1024
#endif
