# See LICENSE for license details.

#*****************************************************************************
# lw.S
#-----------------------------------------------------------------------------
#
# Test lw instruction.
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

  TEST_LD_OP( 2, lw, 0x0000000000ff00ff, 0,  tdat );
  TEST_LD_OP( 3, lw, 0xffffffffff00ff00, 4,  tdat );
  TEST_LD_OP( 4, lw, 0x000000000ff00ff0, 8,  tdat );
  TEST_LD_OP( 5, lw, 0xfffffffff00ff00f, 12, tdat );

  # Test with negative offset

  TEST_LD_OP( 6, lw, 0x0000000000ff00ff, -12, tdat4 );
  TEST_LD_OP( 7, lw, 0xffffffffff00ff00, -8,  tdat4 );
  TEST_LD_OP( 8, lw, 0x000000000ff00ff0, -4,  tdat4 );
  TEST_LD_OP( 9, lw, 0xfffffffff00ff00f, 0,   tdat4 );

  # Test with a negative base

  TEST_CASE( 10, x5, 0x0000000000ff00ff, \
    la  x1, tdat; \
    addi x1, x1, -32; \
    lw x5, 32(x1); \
  )

  # Test with unaligned base

  TEST_CASE( 11, x5, 0xffffffffff00ff00, \
    la  x1, tdat; \
    addi x1, x1, -3; \
    lw x5, 7(x1); \
  )

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_LD_DEST_BYPASS( 12, 0, lw, 0x000000000ff00ff0, 4, tdat2 );
  TEST_LD_DEST_BYPASS( 13, 1, lw, 0xfffffffff00ff00f, 4, tdat3 );
  TEST_LD_DEST_BYPASS( 14, 2, lw, 0xffffffffff00ff00, 4, tdat1 );

  TEST_LD_SRC1_BYPASS( 15, 0, lw, 0x000000000ff00ff0, 4, tdat2 );
  TEST_LD_SRC1_BYPASS( 16, 1, lw, 0xfffffffff00ff00f, 4, tdat3 );
  TEST_LD_SRC1_BYPASS( 17, 2, lw, 0xffffffffff00ff00, 4, tdat1 );

  #-------------------------------------------------------------
  # Test write-after-write hazard
  #-------------------------------------------------------------

  TEST_CASE( 18, x2, 2, \
    la  x5, tdat; \
    lw  x2, 0(x5); \
    li  x2, 2; \
  )

  TEST_CASE( 19, x2, 2, \
    la  x5, tdat; \
    lw  x2, 0(x5); \
    nop; \
    li  x2, 2; \
  )

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
tdat:
tdat1:  .word 0x00ff00ff
tdat2:  .word 0xff00ff00
tdat3:  .word 0x0ff00ff0
tdat4:  .word 0xf00ff00f

RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END


