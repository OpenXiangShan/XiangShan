    .option norvc
    .option nopic
    .section .text
    .globl _start
_start:
    la      t0, trap
    csrw    mtvec, t0
    la      s0, dataset
    li      s1, 16
    li      s2, 0x8e4c17a3d692f5b0
    li      s3, 2
    li      s4, 0x5a5a
    li      s5, 3
    li      s6, 0
    li      s7, PMP_DENY

    mv      t0, s0
    mv      t1, s1
initialize:
    sd      s4, 0(t0)
    addi    t0, t0, 512
    addi    t1, t1, -1
    bnez    t1, initialize
    fence   rw, rw

next_round:
    mv      t3, s2
    mv      t4, s1
load_target:
    andi    t0, t3, 15
    slli    t0, t0, 9
    add     t0, s0, t0
    ld      t1, 0(t0)
    bne     t1, s4, fail
    bnez    s6, check_access

    # Keep the training sequence at one load PC and allow each update to finish.
    slli    t2, t1, 1
    .rept   64
    mul     t2, t2, s5
    .endr
    andi    t2, t2, 1
    or      s0, s0, t2
    srli    t3, t3, 4
    addi    t4, t4, -1
    bnez    t4, load_target
    addi    s3, s3, -1
    bnez    s3, next_round

    # Put the protected targets in their own 4 KiB PMP region.
    li      t0, 4096
    add     s8, s0, t0
    mv      t0, s8
    li      t1, 8
flush_targets:
    cbo.flush 0(t0)
    addi    t0, t0, 512
    addi    t1, t1, -1
    bnez    t1, flush_targets
    fence   rw, rw
    srli    t0, s8, 2
    ori     t0, t0, 511
    csrw    pmpaddr0, t0
    # Locked NAPOT applies in M-mode. Only R differs between the runs.
    li      t0, 0x99
    xor     t0, t0, s7
    csrw    pmpcfg0, t0
    li      s6, 1
    j       next_round

check_access:
    li      t2, 2048
drain:
    addi    t2, t2, -1
    bnez    t2, drain
faulting_load:
    ld      t1, 0(s8)
    bnez    s7, fail
    bne     t1, s4, fail
    j       pass

    .balign 4
trap:
    beqz    s7, fail
    csrr    t0, mcause
    li      t1, 5
    bne     t0, t1, fail
    csrr    t0, mtval
    bne     t0, s8, fail
    csrr    t0, mepc
    la      t1, faulting_load
    bne     t0, t1, fail
pass:
    li      a0, 0
    .word   0x0000006b
    j       .
fail:
    li      a0, 1
    .word   0x0000006b
    j       .

    .section .bss
    .balign 4096
dataset:
    .skip   8192
