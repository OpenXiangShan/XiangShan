    # Name   : fe_2fetch_cross_page_blocked
    # Goal   : force two otherwise short fetch blocks onto different 4 KiB pages

    .section .text, "ax"
    .option rvc
    .globl _start

_start:
    addi s0, zero, 32
    j page_tail

    # This block starts at page+0xfe0 and ends with a direct jump to page+0x1000.
    # Its successor is therefore a valid short block in the next virtual page.
    # The following short-block chain gives BPU enough lookahead to separate
    # the page-boundary blocker from the runahead-distance blocker.
    .org 0xfe0
    .balign 32
page_tail:
    .rept 14
    c.nop
    .endr
    # jal x0, +4: keep this transfer exactly 4 bytes at page+0xffc.
    .word 0x0040006f

page_next:
    .rept 15
    c.nop
    .endr
    c.j block2

    .balign 32
block2:
    .rept 15
    c.nop
    .endr
    c.j block3

    .balign 32
block3:
    .rept 15
    c.nop
    .endr
    c.j block4

    .balign 32
block4:
    .rept 15
    c.nop
    .endr
    c.j loop_tail

    .balign 32
loop_tail:
    .rept 13
    c.nop
    .endr
    c.addi s0, -1
    c.bnez s0, page_tail
    c.nop

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 128
    c.nop
    .endr
