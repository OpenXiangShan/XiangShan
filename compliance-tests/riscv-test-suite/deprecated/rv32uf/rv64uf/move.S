# See LICENSE for license details.

#*****************************************************************************
# move.S
#-----------------------------------------------------------------------------
#
# This test verifies that the fmv.s.x, fmv.x.s, and fsgnj[x|n].d instructions
# and the fcsr work properly.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"

RVTEST_RV64UF
RV_COMPLIANCE_CODE_BEGIN

  TEST_CASE(2, a1, 1, csrwi fcsr, 1; li a0, 0x1234; fssr a1, a0)
  TEST_CASE(3, a0, 0x34, frsr a0)
  TEST_CASE(4, a0, 0x14, frflags a0)
  TEST_CASE(5, a0, 0x01, csrrwi a0, frm, 2)
  TEST_CASE(6, a0, 0x54, frsr a0)
  TEST_CASE(7, a0, 0x14, csrrci a0, fflags, 4)
  TEST_CASE(8, a0, 0x50, frsr a0)

#define TEST_FSGNJS(n, insn, new_sign, rs1_sign, rs2_sign) \
  TEST_CASE(n, a0, 0x12345678 | (-(new_sign) << 31), \
    li a1, ((rs1_sign) << 31) | 0x12345678; \
    li a2, -(rs2_sign); \
    fmv.s.x f1, a1; \
    fmv.s.x f2, a2; \
    insn f0, f1, f2; \
    fmv.x.s a0, f0)

  TEST_FSGNJS(10, fsgnj.s, 0, 0, 0)
  TEST_FSGNJS(11, fsgnj.s, 1, 0, 1)
  TEST_FSGNJS(12, fsgnj.s, 0, 1, 0)
  TEST_FSGNJS(13, fsgnj.s, 1, 1, 1)

  TEST_FSGNJS(20, fsgnjn.s, 1, 0, 0)
  TEST_FSGNJS(21, fsgnjn.s, 0, 0, 1)
  TEST_FSGNJS(22, fsgnjn.s, 1, 1, 0)
  TEST_FSGNJS(23, fsgnjn.s, 0, 1, 1)

  TEST_FSGNJS(30, fsgnjx.s, 0, 0, 0)
  TEST_FSGNJS(31, fsgnjx.s, 1, 0, 1)
  TEST_FSGNJS(32, fsgnjx.s, 1, 1, 0)
  TEST_FSGNJS(33, fsgnjx.s, 0, 1, 1)

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

