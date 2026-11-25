    .option norvc
    .option nopic
    .section .text
    .globl _start
_start:
    la      t0, fail
    csrw    mtvec, t0
#if defined(ACCESS_MMIO)
    li      t1, 0x38000000
#else
    la      t1, word
    sw      zero, 0(t1)
#endif
    li      t2, 64
    fence   rw, rw
read_word:
    lw      t3, 0(t1)
    bnez    t3, fail
    fence   r, r
    addi    t2, t2, -1
    bnez    t2, read_word

    li      a0, 0
    .word   0x0000006b
    j       .

    .balign 4
fail:
    li      a0, 1
    .word   0x0000006b
    j       .

    .section .bss
    .balign 64
word:
    .skip   64
