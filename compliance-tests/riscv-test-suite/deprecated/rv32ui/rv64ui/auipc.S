# See LICENSE for license details.

#*****************************************************************************
# auipc.S
#-----------------------------------------------------------------------------
#
# Test auipc instruction.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  TEST_CASE(2, a0, 10000, \
    .align 3; \
    lla a0, 1f + 10000; \
    jal a1, 1f; \
    1: sub a0, a0, a1; \
  )

  TEST_CASE(3, a0, -10000, \
    .align 3; \
    lla a0, 1f - 10000; \
    jal a1, 1f; \
    1: sub a0, a0, a1; \
  )

  TEST_PASSFAIL

  RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 4, 4, -1
RV_COMPLIANCE_DATA_END

