    # Name   : fe_ifu_predecode_full
    # Goal   : exercise IFU PreDecode type, offset, call, and return metadata

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi s0, zero, 24

    .balign 32
loop_head:
    addi s1, s1, 1
    bne s1, zero, branch_target
    nop

branch_target:
    jal ra, direct_call
.Lindirect_hi:
    auipc t0, %pcrel_hi(indirect_call)
    addi t0, t0, %pcrel_lo(.Lindirect_hi)
    jalr ra, 0(t0)
after_indirect_call:
    addi s0, s0, -1
    bne s0, zero, loop_head
    jal zero, done

direct_call:
    addi s2, s2, 1
    jalr zero, 0(ra)

indirect_call:
    addi s3, s3, 1
    jalr zero, 0(ra)

done:
    addi a0, zero, 0
    .word 0x0005006b
    .rept 128
    nop
    .endr
