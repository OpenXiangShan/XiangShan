    # Name   : fe_ifu_rvc_seq_boundary
    # Goal   : directed IFU RVC boundary/predecode coverage on normal icache path
    # Covers : RVC 2B PC step, continuous RVC slots, mixed RVI/RVC PC sequence,
    #          fetch-block head/mid/tail valid positions

    .section .text, "ax"
    .option rvc
    .globl _start

_start:
    c.nop
    c.addi s0, 1
    c.addi s1, 1
    c.addi s2, 1
    .option push
    .option norvc
    addi s3, zero, 3
    .option pop

    .rept 10
    c.nop
    .endr
mid_rvc_region:
    c.nop
    c.addi s4, 1
    c.addi s5, 1
    .option push
    .option norvc
    addi s6, zero, 6
    .option pop

    .rept 9
    c.nop
    .endr
tail_rvc_region:
    c.nop
    c.addi s7, 1

next_block_head:
    c.nop
    c.addi s8, 1
    .option push
    .option norvc
    addi s9, zero, 9
    .option pop

done:
    .option push
    .option norvc
    addi a0, zero, 0
    .word 0x0005006b
    .rept 128
    nop
    .endr
    .option pop
