# See LICENSE for license details.

#*****************************************************************************
# wfi.S
#-----------------------------------------------------------------------------
#
# Test wait-for-interrupt instruction.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64S
RV_COMPLIANCE_CODE_BEGIN

  # Make sure wfi doesn't halt the hart, even if interrupts are disabled
  csrc sstatus, SSTATUS_SIE
  csrs sie, SIP_SSIP
  csrs sip, SIP_SSIP

  li TESTNUM, 1
  SWSIG(1, TESTNUM)
  wfi
  li TESTNUM, 2
  SWSIG(2, TESTNUM)

  RV_COMPLIANCE_HALT

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

