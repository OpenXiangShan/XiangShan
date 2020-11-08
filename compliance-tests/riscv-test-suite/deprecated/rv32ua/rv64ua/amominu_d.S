# See LICENSE for license details.

#*****************************************************************************
# amominu_d.S
#-----------------------------------------------------------------------------
#
# Test amominu.d instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  TEST_CASE(2, a4, 0xffffffff80000000, \
    li a0, 0xffffffff80000000; \
    li a1, 0xfffffffffffff800; \
    la a3, amo_operand; \
    sd a0, 0(a3); \
    amominu.d	a4, a1, 0(a3); \
  )

  TEST_CASE(3, a5, 0xffffffff80000000, ld a5, 0(a3))

  TEST_CASE(4, a4, 0, \
    li a1, 0xffffffffffffffff; \
    sd x0, 0(a3); \
    amominu.d	a4, a1, 0(a3); \
  )

  TEST_CASE(5, a5, 0, ld a5, 0(a3))

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END

  .bss
  .align 3
amo_operand:
  .dword 0
  
