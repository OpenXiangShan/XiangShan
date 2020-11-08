# See LICENSE for license details.

#*****************************************************************************
# move.S
#-----------------------------------------------------------------------------
#
# This test verifies that fmv.d.x, fmv.x.d, and fsgnj[x|n].d work properly.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64UF
RV_COMPLIANCE_CODE_BEGIN

#TODO: make 32-bit compatible version
#define TEST_FSGNJD(n, insn, new_sign, rs1_sign, rs2_sign) \
  TEST_CASE(n, a0, 0x123456789abcdef0 | (-(new_sign) << 63), \
    li a1, ((rs1_sign) << 63) | 0x123456789abcdef0; \
    li a2, -(rs2_sign); \
    fmv.d.x f1, a1; \
    fmv.d.x f2, a2; \
    insn f0, f1, f2; \
    fmv.x.d a0, f0)

  TEST_FSGNJD(10, fsgnj.d, 0, 0, 0)
  TEST_FSGNJD(11, fsgnj.d, 1, 0, 1)
  TEST_FSGNJD(12, fsgnj.d, 0, 1, 0)
  TEST_FSGNJD(13, fsgnj.d, 1, 1, 1)

  TEST_FSGNJD(20, fsgnjn.d, 1, 0, 0)
  TEST_FSGNJD(21, fsgnjn.d, 0, 0, 1)
  TEST_FSGNJD(22, fsgnjn.d, 1, 1, 0)
  TEST_FSGNJD(23, fsgnjn.d, 0, 1, 1)

  TEST_FSGNJD(30, fsgnjx.d, 0, 0, 0)
  TEST_FSGNJD(31, fsgnjx.d, 1, 0, 1)
  TEST_FSGNJD(32, fsgnjx.d, 1, 1, 0)
  TEST_FSGNJD(33, fsgnjx.d, 0, 1, 1)

// Test fsgnj.s in conjunction with double-precision moves
#define TEST_FSGNJS(n, rd, rs1, rs2) \
  TEST_CASE(n, a0, (rd) | (-((rd) >> 31) << 32), \
    li a1, rs1; \
    li a2, rs2; \
    fmv.d.x f1, a1; \
    fmv.d.x f2, a2; \
    fsgnj.s f0, f1, f2; \
    fmv.x.s a0, f0); \
  TEST_CASE(1##n, a0, (rd) | 0xffffffff00000000, \
    li a1, rs1; \
    li a2, rs2; \
    fmv.d.x f1, a1; \
    fmv.d.x f2, a2; \
    fsgnj.s f0, f1, f2; \
    fmv.x.d a0, f0)

  TEST_FSGNJS(40, 0x7fc00000, 0x7ffffffe12345678, 0)
  TEST_FSGNJS(41, 0x7fc00000, 0xfffffffe12345678, 0)
  TEST_FSGNJS(42, 0x7fc00000, 0x7fffffff12345678, 0)
  TEST_FSGNJS(43, 0x12345678, 0xffffffff12345678, 0)

  TEST_FSGNJS(50, 0x7fc00000, 0x7ffffffe12345678, 0x80000000)
  TEST_FSGNJS(51, 0x7fc00000, 0xfffffffe12345678, 0x80000000)
  TEST_FSGNJS(52, 0x7fc00000, 0x7fffffff12345678, 0x80000000)
  TEST_FSGNJS(53, 0x12345678, 0xffffffff12345678, 0x80000000)

  TEST_FSGNJS(60, 0xffc00000, 0x7ffffffe12345678, 0xffffffff80000000)
  TEST_FSGNJS(61, 0xffc00000, 0xfffffffe12345678, 0xffffffff80000000)
  TEST_FSGNJS(62, 0x92345678, 0xffffffff12345678, 0xffffffff80000000)
  TEST_FSGNJS(63, 0x12345678, 0xffffffff12345678, 0x7fffffff80000000)

// Test fsgnj.d in conjunction with single-precision moves
#define TEST_FSGNJD_SP(n, isnan, rd, rs1, rs2) \
  TEST_CASE(n, a0, ((rd) & 0xffffffff) | (-(((rd) >> 31) & 1) << 32), \
    li a1, rs1; \
    li a2, rs2; \
    fmv.d.x f1, a1; \
    fmv.d.x f2, a2; \
    fsgnj.d f0, f1, f2; \
    feq.s a0, f0, f0; \
    addi a0, a0, -!(isnan); \
    bnez a0, 1f; \
    fmv.x.s a0, f0; \
    1:); \
  TEST_CASE(1##n, a0, rd, \
    li a1, rs1; \
    li a2, rs2; \
    fmv.d.x f1, a1; \
    fmv.d.x f2, a2; \
    fsgnj.d f0, f1, f2; \
    fmv.x.d a0, f0; \
    1:)

  TEST_FSGNJD_SP(70, 0, 0xffffffff11111111, 0xffffffff11111111, 0xffffffff11111111)
  TEST_FSGNJD_SP(71, 1, 0x7fffffff11111111, 0xffffffff11111111, 0x7fffffff11111111)
  TEST_FSGNJD_SP(72, 0, 0xffffffff11111111, 0xffffffff11111111, 0xffffffff91111111)
  TEST_FSGNJD_SP(73, 0, 0xffffffff11111111, 0xffffffff11111111, 0x8000000000000000)
  TEST_FSGNJD_SP(74, 0, 0xffffffff11111111, 0x7fffffff11111111, 0xffffffff11111111)
  TEST_FSGNJD_SP(75, 1, 0x7fffffff11111111, 0x7fffffff11111111, 0x7fffffff11111111)
  TEST_FSGNJD_SP(76, 0, 0xffffffff11111111, 0x7fffffff11111111, 0xffffffff91111111)
  TEST_FSGNJD_SP(77, 0, 0xffffffff11111111, 0x7fffffff11111111, 0x8000000000000000)

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
