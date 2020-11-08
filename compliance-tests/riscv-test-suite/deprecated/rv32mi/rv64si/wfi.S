# See LICENSE for license details.

#*****************************************************************************
# wfi.S
#-----------------------------------------------------------------------------
#
# Test wait-for-interrupt instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64S
RV_COMPLIANCE_CODE_BEGIN

  # Make sure wfi doesn't halt the hart, even if interrupts are disabled
  csrc sstatus, SSTATUS_SIE
  csrs sie, SIP_SSIP
  csrs sip, SIP_SSIP
  wfi

  RV_COMPLIANCE_HALT

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
