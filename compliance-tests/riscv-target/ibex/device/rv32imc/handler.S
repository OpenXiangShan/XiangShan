.section .text.trap;
.align  4;

_trap_start:
    j _trap_exception
    j _int_exc
    j _int_exc
    j _int_exc
    j _int_exc
    j _int_exc
    j _int_exc
    j _int_exc
    j _int_exc
    j _int_exc
    j _int_exc
    j _int_exc
    j _int_exc


// This could be exception or user interrupt
// 0xb is the environment call to indicate the end
_trap_exception:
    csrr a0, mcause;
    addi a1, zero, 0xb
    beq a0, a1, 1f
    la a1, begin_signature
    sw a0, 0(a1);
1:
    la a0, write_tohost;
    jr a0;

_int_exc:
    la a0, write_tohost;
    jr a0;

