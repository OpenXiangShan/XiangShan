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
    sd      s4, 0(s2)

    mv      t0, s0
    mv      t1, s3
    addi    t2, s1, 16
initialize:
    sd      t2, 8(t0)
    sd      s2, -16(t2)
    addi    t2, t2, 64
    addi    t0, t0, 64
    addi    t1, t1, -1
    bnez    t1, initialize
    fence   rw, rw

    li      s6, 3
round:
    # Train stable address pairs before the sequential pass activates the spatial source.
    li      s9, 41
    li      t0, 1
    bne     s6, t0, set_stride
    li      s9, 1
set_stride:
    mv      t0, s0
    mv      t1, s3
    mv      t4, s1
flush_producers:
    cbo.flush 0(t0)
    cbo.flush 0(t4)
    addi    t0, t0, 64
    addi    t4, t4, 64
    addi    t1, t1, -1
    bnez    t1, flush_producers
    fence   rw, rw

    li      s7, 0
    mv      s8, s3
sequential_loads:
    slli    t3, s7, 6
    add     t3, s0, t3
    ld      t0, 8(t3)
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
    add     s7, s7, s9
    andi    s7, s7, 127
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
    .skip   8192
    .balign 4096
leaf:
    .skip   64
