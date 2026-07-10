    # Name        : fe_baremode_python_pilot_mix
    # Goal        : bin-trace equivalent for program-only Python baremode pilots
    # Entry       : _start at 0x80000000
    # Source      : test_functional_coverage_baremode.py
    # Covers      : sequential icache fetch, direct JAL, conditional not-taken branch
    # Not covered : backend injected redirect/fault/backpressure pilots; those need env control
    # Check       : NEMU reaches GOOD TRAP with a0=0

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    # Match the original baremode normal-path base address.
    addi s0, zero, 0
    addi s1, zero, 1
    addi s2, zero, 0
    addi s3, zero, 0

    # Sequential icache stream, corresponding to the old NOP-only warmup.
    nop
    nop
    nop
    nop

    # Direct jump coverage: Python pilot used jal x0, +8 at pc 0.
direct_jmp_site:
    jal zero, direct_jmp_target
    addi s0, s0, -1
direct_jmp_target:
    addi s0, s0, 1

    # Conditional branch not-taken coverage: Python pilot used bne x0, x0, +8.
cond_nt_site:
    bne zero, zero, cond_nt_target
    addi s2, s2, 1
cond_nt_target:
    addi s3, s3, 1

    # A short straight-line tail keeps the frontend on the normal icache path
    # long enough for bin-trace completion and coverage sampling.
    nop
    nop
    nop
    nop

done:
    addi a0, zero, 0
    .word 0x0005006b

