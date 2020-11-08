# See LICENSE for license details.

#*****************************************************************************
# jal.S
#-----------------------------------------------------------------------------
#
# Test jal instruction.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"



RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Test 2: Basic test
  #-------------------------------------------------------------

test_2:
  li  TESTNUM, 2
  li  ra, 0

  jal x4, target_2
linkaddr_2:
  nop
  nop

  j fail

target_2:
  la  x2, linkaddr_2
  bne x2, x4, fail

  #-------------------------------------------------------------
  # Test delay slot instructions not executed nor bypassed
  #-------------------------------------------------------------

  TEST_CASE( 3, ra, 3, \
    li  ra, 1; \
    jal x0, 1f; \
    addi ra, ra, 1; \
    addi ra, ra, 1; \
    addi ra, ra, 1; \
    addi ra, ra, 1; \
1:  addi ra, ra, 1; \
    addi ra, ra, 1; \
  )

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 10, 4, -1
RV_COMPLIANCE_DATA_END

