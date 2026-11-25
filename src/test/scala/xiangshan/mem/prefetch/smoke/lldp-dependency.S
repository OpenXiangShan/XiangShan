    .option norvc
    .option nopic
    .section .text
    .globl _start
_start:
    la      t0, fail
    csrw    mtvec, t0
    la      s0, producer
    la      s1, consumer
    li      s2, 0x4000
    li      s3, 16
    li      s4, 0x5a5a
    li      s5, -16

    mv      t0, s0
    mv      t1, s1
    mv      t2, s3
initialize:
    addi    t3, t1, 16
    sd      t3, 8(t0)
    sd      s4, 0(t1)
    add     t0, t0, s2
    add     t1, t1, s2
    addi    t2, t2, -1
    bnez    t2, initialize
    fence   rw, rw

    li      s6, 16
round:
    mv      s7, s0
    mv      s8, s3
dependence:
    ld      t0, 8(s7)
#if defined(LONG_CHAIN)
    addi    t0, t0, 1
    addi    t0, t0, 2
    addi    t0, t0, -19
#elif defined(TWO_SOURCES)
    add     t0, t0, s5
#else
    addi    t0, t0, -16
#endif
    ld      t1, 0(t0)
    bne     t1, s4, fail
    add     s7, s7, s2
    addi    s8, s8, -1
    bnez    s8, dependence
    addi    s6, s6, -1
    bnez    s6, round

    li      a0, 0
    .word   0x0000006b
    j       .

    .balign 4
fail:
    li      a0, 1
    .word   0x0000006b
    j       .

    .section .bss
    .balign 16384
producer:
    .skip   262144
consumer:
    .skip   262144
