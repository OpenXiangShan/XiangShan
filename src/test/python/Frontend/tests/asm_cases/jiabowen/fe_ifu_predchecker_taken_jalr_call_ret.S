    # Name   : fe_ifu_predchecker_taken_jalr_call_ret
    # Goal   : train taken RVI JALR, CALL and RET forms in one repeated path.

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi s0, zero, 48

loop:
    # RVI CALL: rd=x1, with the target returning through x1.
.Lcall_hi:
    auipc t0, %pcrel_hi(call_target)
    addi t0, t0, %pcrel_lo(.Lcall_hi)
    jalr ra, 0(t0)

after_call:
    # RVI plain JALR: rd=x0, target falls back to the loop continuation.
.Ljalr_hi:
    auipc t0, %pcrel_hi(jalr_target)
    addi t0, t0, %pcrel_lo(.Ljalr_hi)
    jalr zero, 0(t0)

after_jalr:
    addi s0, s0, -1
    bnez s0, loop

done:
    addi a0, zero, 0
    .word 0x0005006b
    .balign 64
    .rept 128
    nop
    .endr

call_target:
    # RVI RET: rd=x0, rs1=x1, returning to after_call.
    jalr zero, 0(ra)

jalr_target:
    jal zero, after_jalr
