    .option norvc
    .option nopic
    .section .text
    .globl _start
_start:
    la      t0, fail
    csrw    mtvec, t0
    la      s0, producers
    la      s1, consumer
    la      s2, leaf
    li      s3, 128
    li      s4, 0x5a5a
    li      s5, 3
    sd      s2, 0(s1)
    sd      s4, 0(s2)

    mv      t0, s0
    mv      t1, s3
    addi    t2, s1, 16
initialize:
    sd      t2, 8(t0)
    addi    t0, t0, 64
    addi    t1, t1, -1
    bnez    t1, initialize
    fence   rw, rw

    li      s6, 2
round:
    mv      t0, s0
    mv      t1, s3
flush_producers:
    cbo.flush 0(t0)
    addi    t0, t0, 64
    addi    t1, t1, -1
    bnez    t1, flush_producers
    fence   rw, rw

    mv      s7, s0
    mv      s8, s3
sequential_loads:
    ld      t0, 8(s7)
    addi    t0, t0, -16
    ld      t1, 0(t0)
    ld      t2, 0(t1)
    bne     t2, s4, fail

    # Leave room for a spatial request and its refill before the next producer.
    slli    t2, t2, 1
    .rept   40
    mul     t2, t2, s5
    .endr
    andi    t2, t2, 1
    add     s7, s7, t2
    addi    s7, s7, 64
    addi    s8, s8, -1
    bnez    s8, sequential_loads
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
    .balign 4096
producers:
    .skip   8192
    .balign 4096
consumer:
    .skip   64
    .balign 4096
leaf:
    .skip   64
