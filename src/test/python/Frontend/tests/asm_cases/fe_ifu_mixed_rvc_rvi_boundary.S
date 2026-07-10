    # Name   : fe_ifu_mixed_rvc_rvi_boundary
    # Goal   : directed mixed RVC/RVI IFU boundary coverage
    # Covers : mixed no-gap/no-dup PC stream, RVI high-half suppression,
    #          RVC/RVI at fetch-block head/mid/tail

    .section .text, "ax"
    .option rvc
    .globl _start

_start:
    c.nop
    .option push
    .option norvc
    addi s0, zero, 0
    .option pop
    c.addi s0, 1
    .option push
    .option norvc
    addi s1, zero, 1
    .option pop
    c.addi s1, 1

    .rept 8
    c.nop
    .endr
cross_32b_mid:
    .option push
    .option norvc
    addi s2, zero, 2
    .option pop
    c.nop
    .option push
    .option norvc
    addi s3, zero, 3
    .option pop

    .rept 9
    c.nop
    .endr
tail_mixed_region:
    c.nop
    .option push
    .option norvc
    addi s4, zero, 4
    .option pop
    c.addi s4, 1

next_block_head:
    .option push
    .option norvc
    addi s5, zero, 5
    .option pop
    c.nop
    .option push
    .option norvc
    addi s6, zero, 6
    .option pop
    c.addi s6, 1

done:
    .option push
    .option norvc
    addi a0, zero, 0
    .word 0x0005006b
    .rept 128
    nop
    .endr
    .option pop
