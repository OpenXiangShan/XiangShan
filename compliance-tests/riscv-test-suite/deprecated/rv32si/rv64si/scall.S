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
  #undef SSTATUS_SPP
  #define SSTATUS_SPP MSTATUS_MPP
#endif

  li TESTNUM, 2
  SWSIG(2, TESTNUM)

  # This is the expected trap code.
  li t1, CAUSE_USER_ECALL

#ifdef __MACHINE_MODE
  # If running in M mode, use mstatus.MPP to check existence of U mode.
  # Otherwise, if in S mode, then U mode must exist and we don't need to check.
  li t0, MSTATUS_MPP
  csrc mstatus, t0
  csrr t2, mstatus
  and t0, t0, t2
  beqz t0, 1f

  # If U mode doesn't exist, mcause should indicate ECALL from M mode.
  li t1, CAUSE_MACHINE_ECALL
#endif

1:
  li t0, SSTATUS_SPP
  csrc sstatus, t0
  la t0, 1f
  csrw sepc, t0
  sret
1:

  li TESTNUM, 1
  SWSIG(1, TESTNUM)
do_scall:
  scall
  j fail

  TEST_PASSFAIL

  .align 2
  .global stvec_handler
stvec_handler:
  csrr t0, scause
  bne t0, t1, fail
  la t2, do_scall
  csrr t0, sepc
  bne t0, t2, fail
  j pass

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

