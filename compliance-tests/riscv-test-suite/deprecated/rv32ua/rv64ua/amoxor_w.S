# See LICENSE for license details.

#*****************************************************************************
# amoxor_w.S
#-----------------------------------------------------------------------------
#
# Test amoxor.w instruction.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"



RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  TEST_CASE(2, a4, 0xffffffff80000000, \
    li a0, 0xffffffff80000000; \
    li a1, 0xfffffffffffff800; \
    la a3, amo_operand; \
    sw a0, 0(a3); \
    amoxor.w	a4, a1, 0(a3); \
  )

  TEST_CASE(3, a5, 0x7ffff800, lw a5, 0(a3))

  # try again after a cache miss
  TEST_CASE(4, a4, 0x7ffff800, \
    li  a1, 0xc0000001; \
    amoxor.w a4, a1, 0(a3); \
  )

  TEST_CASE(5, a5, 0xffffffffbffff801, lw a5, 0(a3))

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .bss
  .align 3
amo_operand:
  .dword 0


  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

