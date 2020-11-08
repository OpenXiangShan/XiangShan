# See LICENSE for license details.

#*****************************************************************************
# sb.S
#-----------------------------------------------------------------------------
#
# Test sb instruction.
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

  TEST_ST_OP( 2, lb, sb, 0xffffffffffffffaa, 0, tdat );
  TEST_ST_OP( 3, lb, sb, 0x0000000000000000, 1, tdat );
  TEST_ST_OP( 4, lh, sb, 0xffffffffffffefa0, 2, tdat );
  TEST_ST_OP( 5, lb, sb, 0x000000000000000a, 3, tdat );

  # Test with negative offset

  TEST_ST_OP( 6, lb, sb, 0xffffffffffffffaa, -3, tdat8 );
  TEST_ST_OP( 7, lb, sb, 0x0000000000000000, -2, tdat8 );
  TEST_ST_OP( 8, lb, sb, 0xffffffffffffffa0, -1, tdat8 );
  TEST_ST_OP( 9, lb, sb, 0x000000000000000a, 0,  tdat8 );

  # Test with a negative base

  TEST_CASE( 10, x5, 0x78, \
    la  x1, tdat9; \
    li  x2, 0x12345678; \
    addi x4, x1, -32; \
    sb x2, 32(x4); \
    lb x5, 0(x1); \
  )

  # Test with unaligned base

  TEST_CASE( 11, x5, 0xffffffffffffff98, \
    la  x1, tdat9; \
    li  x2, 0x00003098; \
    addi x1, x1, -6; \
    sb x2, 7(x1); \
    la  x4, tdat10; \
    lb x5, 0(x4); \
  )

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_ST_SRC12_BYPASS( 12, 0, 0, lb, sb, 0xffffffffffffffdd, 0, tdat );
  TEST_ST_SRC12_BYPASS( 13, 0, 1, lb, sb, 0xffffffffffffffcd, 1, tdat );
  TEST_ST_SRC12_BYPASS( 14, 0, 2, lb, sb, 0xffffffffffffffcc, 2, tdat );
  TEST_ST_SRC12_BYPASS( 15, 1, 0, lb, sb, 0xffffffffffffffbc, 3, tdat );
  TEST_ST_SRC12_BYPASS( 16, 1, 1, lb, sb, 0xffffffffffffffbb, 4, tdat );
  TEST_ST_SRC12_BYPASS( 17, 2, 0, lb, sb, 0xffffffffffffffab, 5, tdat );

  TEST_ST_SRC21_BYPASS( 18, 0, 0, lb, sb, 0x33, 0, tdat );
  TEST_ST_SRC21_BYPASS( 19, 0, 1, lb, sb, 0x23, 1, tdat );
  TEST_ST_SRC21_BYPASS( 20, 0, 2, lb, sb, 0x22, 2, tdat );
  TEST_ST_SRC21_BYPASS( 21, 1, 0, lb, sb, 0x12, 3, tdat );
  TEST_ST_SRC21_BYPASS( 22, 1, 1, lb, sb, 0x11, 4, tdat );
  TEST_ST_SRC21_BYPASS( 23, 2, 0, lb, sb, 0x01, 5, tdat );

  li a0, 0xef
  la a1, tdat
  sb a0, 3(a1)

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
tdat:
tdat1:  .byte 0xef
tdat2:  .byte 0xef
tdat3:  .byte 0xef
tdat4:  .byte 0xef
tdat5:  .byte 0xef
tdat6:  .byte 0xef
tdat7:  .byte 0xef
tdat8:  .byte 0xef
tdat9:  .byte 0xef
tdat10: .byte 0xef

RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END
