# See LICENSE for license details.

#*****************************************************************************
# fcvt_w.S
#-----------------------------------------------------------------------------
#
# Test fcvt{wu|w|lu|l}.d instructions.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"

RVTEST_RV64UF
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_FP_INT_OP_D( 2,  fcvt.w.d, 0x01,         -1, -1.1, rtz);
  TEST_FP_INT_OP_D( 3,  fcvt.w.d, 0x00,         -1, -1.0, rtz);
  TEST_FP_INT_OP_D( 4,  fcvt.w.d, 0x01,          0, -0.9, rtz);
  TEST_FP_INT_OP_D( 5,  fcvt.w.d, 0x01,          0,  0.9, rtz);
  TEST_FP_INT_OP_D( 6,  fcvt.w.d, 0x00,          1,  1.0, rtz);
  TEST_FP_INT_OP_D( 7,  fcvt.w.d, 0x01,          1,  1.1, rtz);
  TEST_FP_INT_OP_D( 8,  fcvt.w.d, 0x10,     -1<<31, -3e9, rtz);
  TEST_FP_INT_OP_D( 9,  fcvt.w.d, 0x10,  (1<<31)-1,  3e9, rtz);

  TEST_FP_INT_OP_D(12, fcvt.wu.d, 0x10,          0, -3.0, rtz);
  TEST_FP_INT_OP_D(13, fcvt.wu.d, 0x10,          0, -1.0, rtz);
  TEST_FP_INT_OP_D(14, fcvt.wu.d, 0x01,          0, -0.9, rtz);
  TEST_FP_INT_OP_D(15, fcvt.wu.d, 0x01,          0,  0.9, rtz);
  TEST_FP_INT_OP_D(16, fcvt.wu.d, 0x00,          1,  1.0, rtz);
  TEST_FP_INT_OP_D(17, fcvt.wu.d, 0x01,          1,  1.1, rtz);
  TEST_FP_INT_OP_D(18, fcvt.wu.d, 0x10,          0, -3e9, rtz);
  TEST_FP_INT_OP_D(19, fcvt.wu.d, 0x00, 0xffffffffb2d05e00, 3e9, rtz);

#if __riscv_xlen >= 64
  TEST_FP_INT_OP_D(22,  fcvt.l.d, 0x01,         -1, -1.1, rtz);
  TEST_FP_INT_OP_D(23,  fcvt.l.d, 0x00,         -1, -1.0, rtz);
  TEST_FP_INT_OP_D(24,  fcvt.l.d, 0x01,          0, -0.9, rtz);
  TEST_FP_INT_OP_D(25,  fcvt.l.d, 0x01,          0,  0.9, rtz);
  TEST_FP_INT_OP_D(26,  fcvt.l.d, 0x00,          1,  1.0, rtz);
  TEST_FP_INT_OP_D(27,  fcvt.l.d, 0x01,          1,  1.1, rtz);
  TEST_FP_INT_OP_D(28,  fcvt.l.d, 0x00,-3000000000, -3e9, rtz);
  TEST_FP_INT_OP_D(29,  fcvt.l.d, 0x00, 3000000000,  3e9, rtz);
  TEST_FP_INT_OP_D(20,  fcvt.l.d, 0x10,     -1<<63,-3e19, rtz);
  TEST_FP_INT_OP_D(21,  fcvt.l.d, 0x10,  (1<<63)-1, 3e19, rtz);

  TEST_FP_INT_OP_D(32, fcvt.lu.d, 0x10,          0, -3.0, rtz);
  TEST_FP_INT_OP_D(33, fcvt.lu.d, 0x10,          0, -1.0, rtz);
  TEST_FP_INT_OP_D(34, fcvt.lu.d, 0x01,          0, -0.9, rtz);
  TEST_FP_INT_OP_D(35, fcvt.lu.d, 0x01,          0,  0.9, rtz);
  TEST_FP_INT_OP_D(36, fcvt.lu.d, 0x00,          1,  1.0, rtz);
  TEST_FP_INT_OP_D(37, fcvt.lu.d, 0x01,          1,  1.1, rtz);
  TEST_FP_INT_OP_D(38, fcvt.lu.d, 0x10,          0, -3e9, rtz);
  TEST_FP_INT_OP_D(39, fcvt.lu.d, 0x00, 3000000000,  3e9, rtz);
#endif

  # test negative NaN, negative infinity conversion
  TEST_CASE(42, x1, 0x000000007fffffff, la x1, tdat_d; fld f1,  0(x1); fcvt.w.d x1, f1)
#if __riscv_xlen >= 64
  TEST_CASE(43, x1, 0x7fffffffffffffff, la x1, tdat_d; fld f1,  0(x1); fcvt.l.d x1, f1)
#endif
  TEST_CASE(44, x1, 0xffffffff80000000, la x1, tdat_d; fld f1, 16(x1); fcvt.w.d x1, f1)
#if __riscv_xlen >= 64
  TEST_CASE(45, x1, 0x8000000000000000, la x1, tdat_d; fld f1, 16(x1); fcvt.l.d x1, f1)
#endif

  # test positive NaN, positive infinity conversion
  TEST_CASE(52, x1, 0x000000007fffffff, la x1, tdat_d; fld f1,  8(x1); fcvt.w.d x1, f1)
#if __riscv_xlen >= 64
  TEST_CASE(53, x1, 0x7fffffffffffffff, la x1, tdat_d; fld f1,  8(x1); fcvt.l.d x1, f1)
#endif
  TEST_CASE(54, x1, 0x000000007fffffff, la x1, tdat_d; fld f1, 24(x1); fcvt.w.d x1, f1)
#if __riscv_xlen >= 64
  TEST_CASE(55, x1, 0x7fffffffffffffff, la x1, tdat_d; fld f1, 24(x1); fcvt.l.d x1, f1)
#endif

  # test NaN, infinity conversions to unsigned integer
  TEST_CASE(62, x1, 0xffffffffffffffff, la x1, tdat_d; fld f1,  0(x1); fcvt.wu.d x1, f1)
  TEST_CASE(63, x1, 0xffffffffffffffff, la x1, tdat_d; fld f1,  8(x1); fcvt.wu.d x1, f1)
  TEST_CASE(64, x1,                  0, la x1, tdat_d; fld f1, 16(x1); fcvt.wu.d x1, f1)
  TEST_CASE(65, x1, 0xffffffffffffffff, la x1, tdat_d; fld f1, 24(x1); fcvt.wu.d x1, f1)
#if __riscv_xlen >= 64
  TEST_CASE(66, x1, 0xffffffffffffffff, la x1, tdat_d; fld f1,  0(x1); fcvt.lu.d x1, f1)
  TEST_CASE(67, x1, 0xffffffffffffffff, la x1, tdat_d; fld f1,  8(x1); fcvt.lu.d x1, f1)
  TEST_CASE(68, x1,                  0, la x1, tdat_d; fld f1, 16(x1); fcvt.lu.d x1, f1)
  TEST_CASE(69, x1, 0xffffffffffffffff, la x1, tdat_d; fld f1, 24(x1); fcvt.lu.d x1, f1)
#endif

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data

  TEST_DATA

# -NaN, NaN, -inf, +inf
tdat:
.word 0xffffffff
.word 0x7fffffff
.word 0xff800000
.word 0x7f800000

tdat_d:
.dword 0xffffffffffffffff
.dword 0x7fffffffffffffff
.dword 0xfff0000000000000
.dword 0x7ff0000000000000

RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

