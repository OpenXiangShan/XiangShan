    # Name   : fe_ifu_predchecker_taken_jalr_call_ret
    # Goal   : train taken RVI JALR, CALL and RET forms in one repeated path.

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi s0, zero, 32

    # Keep the CALL in a fetch block by itself.  x6 is deliberately used as
    # the target base because x5 is also a RAS link register.
.Lcall_setup:
    auipc t1, %pcrel_hi(call_target)
    addi t1, t1, %pcrel_lo(.Lcall_setup)
    jal zero, call_site
    .balign 64

call_site:
    .rept 14
    nop
    .endr
    jalr ra, 0(t1)
    .balign 64

call_after:
    addi s0, s0, -1
    bnez s0, call_site
    jal zero, plain_setup
    .balign 64

call_target:
    # Isolated RET target for the CALL loop.
    jalr zero, 0(ra)
    .rept 15
    nop
    .endr

plain_setup:
    addi s0, zero, 32
.Lplain_addr:
    auipc t1, %pcrel_hi(plain_after)
    addi t1, t1, %pcrel_lo(.Lplain_addr)
    jal zero, plain_source
    .balign 64

plain_source:
    # Isolated plain JALR source; its target block contains only loop control.
    .rept 14
    nop
    .endr
    jalr zero, 0(t1)
    .balign 64

plain_after:
    addi s0, s0, -1
    bnez s0, plain_source

done:
    addi a0, zero, 0
    .word 0x0005006b
    .balign 64
    .rept 128
    nop
    .endr
