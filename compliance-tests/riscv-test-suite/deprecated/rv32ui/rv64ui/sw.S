# See LICENSE for license details.

#*****************************************************************************
# sw.S
#-----------------------------------------------------------------------------
#
# Test sw instruction.
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

  TEST_ST_OP( 2, lw, sw, 0x0000000000aa00aa, 0,  tdat );
  TEST_ST_OP( 3, lw, sw, 0xffffffffaa00aa00, 4,  tdat );
  TEST_ST_OP( 4, lw, sw, 0x000000000aa00aa0, 8,  tdat );
  TEST_ST_OP( 5, lw, sw, 0xffffffffa00aa00a, 12, tdat );

  # Test with negative offset

  TEST_ST_OP( 6, lw, sw, 0x0000000000aa00aa, -12, tdat8 );
  TEST_ST_OP( 7, lw, sw, 0xffffffffaa00aa00, -8,  tdat8 );
  TEST_ST_OP( 8, lw, sw, 0x000000000aa00aa0, -4,  tdat8 );
  TEST_ST_OP( 9, lw, sw, 0xffffffffa00aa00a, 0,   tdat8 );

  # Test with a negative base

  TEST_CASE( 10, x5, 0x12345678, \
    la  x1, tdat9; \
    li  x2, 0x12345678; \
    addi x4, x1, -32; \
    sw x2, 32(x4); \
    lw x5, 0(x1); \
  )

  # Test with unaligned base

  TEST_CASE( 11, x5, 0x58213098, \
    la  x1, tdat9; \
    li  x2, 0x58213098; \
    addi x1, x1, -3; \
    sw x2, 7(x1); \
    la  x4, tdat10; \
    lw x5, 0(x4); \
  )

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_ST_SRC12_BYPASS( 12, 0, 0, lw, sw, 0xffffffffaabbccdd, 0,  tdat );
  TEST_ST_SRC12_BYPASS( 13, 0, 1, lw, sw, 0xffffffffdaabbccd, 4,  tdat );
  TEST_ST_SRC12_BYPASS( 14, 0, 2, lw, sw, 0xffffffffddaabbcc, 8,  tdat );
  TEST_ST_SRC12_BYPASS( 15, 1, 0, lw, sw, 0xffffffffcddaabbc, 12, tdat );
  TEST_ST_SRC12_BYPASS( 16, 1, 1, lw, sw, 0xffffffffccddaabb, 16, tdat );
  TEST_ST_SRC12_BYPASS( 17, 2, 0, lw, sw, 0xffffffffbccddaab, 20, tdat );

  TEST_ST_SRC21_BYPASS( 18, 0, 0, lw, sw, 0x00112233, 0,  tdat );
  TEST_ST_SRC21_BYPASS( 19, 0, 1, lw, sw, 0x30011223, 4,  tdat );
  TEST_ST_SRC21_BYPASS( 20, 0, 2, lw, sw, 0x33001122, 8,  tdat );
  TEST_ST_SRC21_BYPASS( 21, 1, 0, lw, sw, 0x23300112, 12, tdat );
  TEST_ST_SRC21_BYPASS( 22, 1, 1, lw, sw, 0x22330011, 16, tdat );
  TEST_ST_SRC21_BYPASS( 23, 2, 0, lw, sw, 0x12233001, 20, tdat );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data

tdat:
tdat1:  .word 0xdeadbeef
tdat2:  .word 0xdeadbeef
tdat3:  .word 0xdeadbeef
tdat4:  .word 0xdeadbeef
tdat5:  .word 0xdeadbeef
tdat6:  .word 0xdeadbeef
tdat7:  .word 0xdeadbeef
tdat8:  .word 0xdeadbeef
tdat9:  .word 0xdeadbeef
tdat10: .word 0xdeadbeef

RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

