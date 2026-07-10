    # Name   : fe_ifu_cfi_decode_basic
    # Goal   : directed IFU predecode CFI type coverage without env-side redirect stimulus
    # Covers : Non-CFI, Branch, JAL decode classes from normal icache cfVec

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi s0, zero, 0
    nop
    bne zero, zero, branch_not_taken_target
    addi s0, s0, 1

branch_not_taken_target:
    addi s1, zero, 1

    .rept 3
    nop
    .endr
mid_cfi_region:
    nop
    bne zero, zero, branch_not_taken_target_2
    addi s2, zero, 2

branch_not_taken_target_2:
    addi s3, zero, 3
    jal zero, jal_target
    addi s4, zero, -1

jal_target:
    nop
    addi s5, zero, 5

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 128
    nop
    .endr
