    # Name   : fe_2fetch_second_block_taken
    # Goal   : exercise a taken CFI in fetch block one while block zero is
    #          a complete sequential block, forcing V3 firstRange masking.

    .section .text, "ax"
    .option rvc
    .globl _start

_start:
    addi s0, zero, 32

    .balign 64
loop_head:
    # Fetch block zero: no CFI, so both aligned blocks can be presented.
    c.addi s0, -1
    .rept 15
    c.nop
    .endr

    # Fetch block one: the final halfword is the taken CFI.  The last
    # iteration falls through to the termination marker.
    .rept 15
    c.nop
    .endr
    c.bnez s0, loop_head

done:
    .option push
    .option norvc
    addi a0, zero, 0
    .word 0x0005006b
    .option pop

    .rept 128
    c.nop
    .endr
