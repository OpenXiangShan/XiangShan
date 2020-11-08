# See LICENSE for license details.

#*****************************************************************************
# sh.S
#-----------------------------------------------------------------------------
#
# Test sh instruction.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Basic tests
  #-------------------------------------------------------------

  TEST_ST_OP( 2, lh, sh, 0x00000000000000aa, 0, tdat );
  TEST_ST_OP( 3, lh, sh, 0xffffffffffffaa00, 2, tdat );
  TEST_ST_OP( 4, lw, sh, 0xffffffffbeef0aa0, 4, tdat );
  TEST_ST_OP( 5, lh, sh, 0xffffffffffffa00a, 6, tdat );

  # Test with negative offset

  TEST_ST_OP( 6, lh, sh, 0x00000000000000aa, -6, tdat8 );
  TEST_ST_OP( 7, lh, sh, 0xffffffffffffaa00, -4, tdat8 );
  TEST_ST_OP( 8, lh, sh, 0x0000000000000aa0, -2, tdat8 );
  TEST_ST_OP( 9, lh, sh, 0xffffffffffffa00a, 0,  tdat8 );

  # Test with a negative base

  TEST_CASE( 10, x5, 0x5678, \
    la  x1, tdat9; \
    li  x2, 0x12345678; \
    addi x4, x1, -32; \
    sh x2, 32(x4); \
    lh x5, 0(x1); \
  )

  # Test with unaligned base

  TEST_CASE( 11, x5, 0x3098, \
    la  x1, tdat9; \
    li  x2, 0x00003098; \
    addi x1, x1, -5; \
    sh x2, 7(x1); \
    la  x4, tdat10; \
    lh x5, 0(x4); \
  )

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_ST_SRC12_BYPASS( 12, 0, 0, lh, sh, 0xffffffffffffccdd, 0,  tdat );
  TEST_ST_SRC12_BYPASS( 13, 0, 1, lh, sh, 0xffffffffffffbccd, 2,  tdat );
  TEST_ST_SRC12_BYPASS( 14, 0, 2, lh, sh, 0xffffffffffffbbcc, 4,  tdat );
  TEST_ST_SRC12_BYPASS( 15, 1, 0, lh, sh, 0xffffffffffffabbc, 6, tdat );
  TEST_ST_SRC12_BYPASS( 16, 1, 1, lh, sh, 0xffffffffffffaabb, 8, tdat );
  TEST_ST_SRC12_BYPASS( 17, 2, 0, lh, sh, 0xffffffffffffdaab, 10, tdat );

  TEST_ST_SRC21_BYPASS( 18, 0, 0, lh, sh, 0x2233, 0,  tdat );
  TEST_ST_SRC21_BYPASS( 19, 0, 1, lh, sh, 0x1223, 2,  tdat );
  TEST_ST_SRC21_BYPASS( 20, 0, 2, lh, sh, 0x1122, 4,  tdat );
  TEST_ST_SRC21_BYPASS( 21, 1, 0, lh, sh, 0x0112, 6, tdat );
  TEST_ST_SRC21_BYPASS( 22, 1, 1, lh, sh, 0x0011, 8, tdat );
  TEST_ST_SRC21_BYPASS( 23, 2, 0, lh, sh, 0x3001, 10, tdat );

  li a0, 0xbeef
  la a1, tdat
  sh a0, 6(a1)

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
tdat:
tdat1:  .half 0xbeef
tdat2:  .half 0xbeef
tdat3:  .half 0xbeef
tdat4:  .half 0xbeef
tdat5:  .half 0xbeef
tdat6:  .half 0xbeef
tdat7:  .half 0xbeef
tdat8:  .half 0xbeef
tdat9:  .half 0xbeef
tdat10: .half 0xbeef

RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

