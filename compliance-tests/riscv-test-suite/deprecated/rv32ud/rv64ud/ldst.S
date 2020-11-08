# See LICENSE for license details.

#*****************************************************************************
# ldst.S
#-----------------------------------------------------------------------------
#
# This test verifies that flw, fld, fsw, and fsd work properly.
#


#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"

RVTEST_RV64UF
RV_COMPLIANCE_CODE_BEGIN

  la s0, tdat
  TEST_CASE(2, a0, 0x40000000bf800000, fld f2, 0(s0); fsd f2, 16(s0); ld a0, 16(s0))
  TEST_CASE(3, a0, 0x40000000bf800000, fld f2, 0(s0); fsw f2, 16(s0); ld a0, 16(s0))
  TEST_CASE(4, a0, 0x40000000bf800000, flw f2, 0(s0); fsw f2, 16(s0); ld a0, 16(s0))
  TEST_CASE(5, a0, 0xc080000040400000, fld f2, 8(s0); fsd f2, 16(s0); ld a0, 16(s0))
  TEST_CASE(6, a0, 0xffffffff40400000, flw f2, 8(s0); fsd f2, 16(s0); ld a0, 16(s0))

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data

  TEST_DATA

tdat:
.word 0xbf800000
.word 0x40000000
.word 0x40400000
.word 0xc0800000
.word 0xdeadbeef
.word 0xcafebabe
.word 0xabad1dea
.word 0x1337d00d

RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

