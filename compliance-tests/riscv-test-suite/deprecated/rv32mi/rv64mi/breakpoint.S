# See LICENSE for license details.

#*****************************************************************************
# breakpoint.S
#-----------------------------------------------------------------------------
#
# Test breakpoints, if they are implemented.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"

RVTEST_RV64M
RV_COMPLIANCE_CODE_BEGIN

  # Set up breakpoint to trap on M-mode fetches.
  li TESTNUM, 2
  SWSIG(2, TESTNUM)

  # Skip tselect if hard-wired.
  csrw tselect, x0
  csrr a1, tselect
  bne x0, a1, sign_pass

  # Make sure there's a breakpoint there.
  csrr a0, tdata1
  srli a0, a0, __riscv_xlen - 4
  li a1, 2
  bne a0, a1, sign_pass

  la a2, 1f
  csrw tdata2, a2
  li a0, MCONTROL_M | MCONTROL_EXECUTE
  csrw tdata1, a0
  # Skip if breakpoint type is unsupported.
  csrr a1, tdata1
  andi a1, a1, 0x7ff
  bne a0, a1, 2f
  .align 2
1:
  # Trap handler should skip this instruction.
  beqz x0, fail

  # Make sure reads don't trap.
  li TESTNUM, 3
  SWSIG(3, TESTNUM)
  lw a0, (a2)

2:
  # Set up breakpoint to trap on M-mode reads.
  li TESTNUM, 4
  SWSIG(4, TESTNUM)
  li a0, MCONTROL_M | MCONTROL_LOAD
  csrw tdata1, a0
  # Skip if breakpoint type is unsupported.
  csrr a1, tdata1
  andi a1, a1, 0x7ff
  bne a0, a1, 2f
  la a2, data1
  csrw tdata2, a2

  # Trap handler should skip this instruction.
  lw a2, (a2)
  beqz a2, fail

  # Make sure writes don't trap.
  li TESTNUM, 5
  SWSIG(5, TESTNUM)
  sw x0, (a2)

2:
  # Set up breakpoint to trap on M-mode stores.
  li TESTNUM, 6
  SWSIG(6, TESTNUM)
  li a0, MCONTROL_M | MCONTROL_STORE
  csrw tdata1, a0
  # Skip if breakpoint type is unsupported.
  csrr a1, tdata1
  andi a1, a1, 0x7ff
  bne a0, a1, 2f

  # Trap handler should skip this instruction.
  sw a2, (a2)

  # Make sure store didn't succeed.
  li TESTNUM, 7
  SWSIG(7, TESTNUM)
  lw a2, (a2)
  bnez a2, fail

  # Try to set up a second breakpoint.
  li a0, 1
  csrw tselect, a0
  csrr a1, tselect
  bne a0, a1, sign_pass

  # Make sure there's a breakpoint there.
  csrr a0, tdata1
  srli a0, a0, __riscv_xlen - 4
  li a1, 2
  bne a0, a1, sign_pass

  li a0, MCONTROL_M | MCONTROL_LOAD
  csrw tdata1, a0
  la a3, data2
  csrw tdata2, a3

  # Make sure the second breakpoint triggers.
  li TESTNUM, 8
  SWSIG(8, TESTNUM)
  lw a3, (a3)
  beqz a3, fail

  # Make sure the first breakpoint still triggers.
  li TESTNUM, 10
  SWSIG(10, TESTNUM)
  la a2, data1
  sw a2, (a2)
  li TESTNUM, 11
  SWSIG(11, TESTNUM)
  lw a2, (a2)
  bnez a2, fail

2:
  TEST_PASSFAIL

sign_pass:
  li TESTNUM, 3
  SWSIG(3, TESTNUM)
  li TESTNUM, 4
  SWSIG(4, TESTNUM)
  li TESTNUM, 5
  SWSIG(5, TESTNUM)
  li TESTNUM, 6
  SWSIG(6, TESTNUM)
  li TESTNUM, 7
  SWSIG(7, TESTNUM)
  li TESTNUM, 8
  SWSIG(8, TESTNUM)
  li TESTNUM, 10
  SWSIG(10, TESTNUM)
  li TESTNUM, 11
  SWSIG(11, TESTNUM)
  j pass

  .align 2
  .global mtvec_handler
mtvec_handler:
  # Only even-numbered tests should trap.
  andi t0, TESTNUM, 1
  bnez t0, fail

  li t0, CAUSE_BREAKPOINT
  csrr t1, mcause
  bne t0, t1, fail

  csrr t0, mepc
  addi t0, t0, 4
  csrw mepc, t0
  mret

RV_COMPLIANCE_CODE_END

  .data
data1: .word 0
data2: .word 0

RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 32, 4, -1
RV_COMPLIANCE_DATA_END

