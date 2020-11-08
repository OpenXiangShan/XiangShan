# See LICENSE for license details.

#*****************************************************************************
# scall.S
#-----------------------------------------------------------------------------
#
# Test syscall trap.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64S
RV_COMPLIANCE_CODE_BEGIN

#ifdef __MACHINE_MODE
  #define sscratch mscratch
  #define sstatus mstatus
  #define scause mcause
  #define sepc mepc
  #define sret mret
  #define stvec_handler mtvec_handler
#endif

  li TESTNUM, 2
  SWSIG(2, TESTNUM)

do_break:
  sbreak
  j fail

  TEST_PASSFAIL

  .align 2
  .global stvec_handler
stvec_handler:
  li t1, CAUSE_BREAKPOINT
  csrr t0, scause
  bne t0, t1, fail
  la t1, do_break
  csrr t0, sepc
  bne t0, t1, fail
  j pass

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

