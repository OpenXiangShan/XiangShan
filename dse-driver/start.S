/* start.S */
.section entry, "ax"
.globl _start
.type _start, @function

#define MSTATUS_FS 0x00006000

.macro set_reg_zero reg_idx
  mv x\reg_idx, zero
.endm

.macro set_freg_zero freg_idx
  fmv.w.x f\freg_idx, zero
.endm

.macro init_regs
  .altmacro
  .set i, 1
  .rept 31
    set_reg_zero %i
    .set i, i+1
  .endr
.endm


_start:
    init_regs

    mv s0, zero
    la sp, _stack_pointer

    /* 跳转到 main */
    jal ra, main

