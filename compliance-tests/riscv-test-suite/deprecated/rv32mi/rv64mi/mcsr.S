# See LICENSE for license details.

#*****************************************************************************
# mcsr.S
#-----------------------------------------------------------------------------
#
# Test various M-mode CSRs.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"

#RVTEST_RV64S
RVTEST_RV64M

RV_COMPLIANCE_CODE_BEGIN
  # Check that mcpuid reports the correct XLEN
#if __riscv_xlen == 64
  TEST_CASE(2, a0, 0x2, csrr a0, misa; srl a0, a0, 62)
#else
  TEST_CASE(2, a0, 0x1, csrr a0, misa; srl a0, a0, 30)
#endif

  # Check that mhartid reports 0
  TEST_CASE(3, a0, 0x0, csrr a0, mhartid)

  # Check that reading the following CSRs doesn't cause an exception
  csrr a0, mimpid
  csrr a0, marchid
  csrr a0, mvendorid

  # Check that writing hte following CSRs doesn't cause an exception
  li t0, 0
  csrs mtvec, t0
  csrs mepc, t0

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END
