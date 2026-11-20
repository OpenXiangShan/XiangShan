/* Independent finite repair regression; hand-written, no random seed/corpus.
 * Link .text at 0x80000000. First page end is an intentional 32-bit RVI.
 * x1 is written by the real indirect CALL; the real RET returns to after_call.
 */
.section .text
.global _start, recovery_call, after_call, recovery_tail, recovery_ret, good_trap
.option norelax
.option norvc
_start:
    addi x20, x0, 0
    auipc x6, 1                 /* PC=0x80000004 -> 0x80001004 */
    addi x6, x6, -6             /* real RV64 value: 0x80000ffe */
    .rept 13
    addi x0, x0, 0
    .endr
recovery_call:                   /* 0x80000040 */
    jalr x1, 0(x6)              /* cold indirect target, BP controls disabled */
 after_call:                     /* 0x80000044 */
    addi x21, x20, 1
    addi x10, x0, 0
 good_trap:                      /* 0x8000004c: NEMU terminates here */
    .word 0x0005006b
/* Legal wrong-path prefetch padding; never executed on the reference path. */
    .fill (0xffe - (. - _start)) / 2, 2, 0x0001
 recovery_tail:                  /* 0x80000ffe, splits FTQ/page boundary */
    addi x20, x20, 1
 recovery_ret:                   /* 0x80001002, ends at offset2 in next FTQ */
    jalr x0, 0(x1)
    .fill (0x4000 - (. - _start)) / 2, 2, 0x0001
