    .option norvc
    .option nopic
    .section .text
    .globl _start
_start:
    la      t0, fail
    csrw    mtvec, t0
    la      s0, producer
    la      s1, consumer
    la      s2, leaf
    li      s3, 0x4000
    li      s4, 16
    li      s5, 0x5a5a

    mv      t0, s0
    mv      t1, s1
    mv      t2, s2
    mv      t3, s4
initialize:
    addi    t4, t1, 16
    sd      t4, 8(t0)
    sd      t2, 0(t1)
    sd      s5, 0(t2)
    add     t0, t0, s3
    add     t1, t1, s3
    add     t2, t2, s3
    addi    t3, t3, -1
    bnez    t3, initialize
    fence   rw, rw

    li      s6, 32
dependence_round:
    mv      s7, s0
    mv      s8, s4
dependence_load:
    ld      t0, 8(s7)
    addi    t0, t0, -16
    ld      t1, 0(t0)
    ld      t2, 0(t1)
    bne     t2, s5, fail
    add     s7, s7, s3
    addi    s8, s8, -1
    bnez    s8, dependence_load
    addi    s6, s6, -1
    bnez    s6, dependence_round

    li      s6, 48
temporal_round:
    la      s7, permutation
    mv      s8, s4
temporal_load:
    lbu     t0, 0(s7)
    slli    t0, t0, 14
    add     t0, s2, t0
    ld      t1, 0(t0)
    bne     t1, s5, fail
    li      t2, 12
spacing:
    addi    t2, t2, -1
    bnez    t2, spacing
    addi    s7, s7, 1
    addi    s8, s8, -1
    bnez    s8, temporal_load
    addi    s6, s6, -1
    bnez    s6, temporal_round

    li      a0, 0
    .word   0x0000006b
    j       .

    .balign 4
fail:
    li      a0, 1
    .word   0x0000006b
    j       .

    .section .rodata
permutation:
    .byte   0, 11, 5, 15, 2, 9, 6, 13, 3, 10, 7, 1, 12, 4, 14, 8

    .section .bss
    .balign 16384
producer:
    .skip   262144
    .skip   64
consumer:
    .skip   262144
    .skip   64
leaf:
    .skip   262144
