# See LICENSE for license details.

#*****************************************************************************
# lb.S
#-----------------------------------------------------------------------------
#
# Test lb instruction.
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

  TEST_LD_OP( 2, lb, 0xffffffffffffffff, 0,  tdat );
  TEST_LD_OP( 3, lb, 0x0000000000000000, 1,  tdat );
  TEST_LD_OP( 4, lb, 0xfffffffffffffff0, 2,  tdat );
  TEST_LD_OP( 5, lb, 0x000000000000000f, 3, tdat );

  # Test with negative offset

  TEST_LD_OP( 6, lb, 0xffffffffffffffff, -3, tdat4 );
  TEST_LD_OP( 7, lb, 0x0000000000000000, -2,  tdat4 );
  TEST_LD_OP( 8, lb, 0xfffffffffffffff0, -1,  tdat4 );
  TEST_LD_OP( 9, lb, 0x000000000000000f, 0,   tdat4 );

  # Test with a negative base

  TEST_CASE( 10, x5, 0xffffffffffffffff, \
    la  x1, tdat; \
    addi x1, x1, -32; \
    lb x5, 32(x1); \
  )

  # Test with unaligned base

  TEST_CASE( 11, x5, 0x0000000000000000, \
    la  x1, tdat; \
    addi x1, x1, -6; \
    lb x5, 7(x1); \
  )

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_LD_DEST_BYPASS( 12, 0, lb, 0xfffffffffffffff0, 1, tdat2 );
  TEST_LD_DEST_BYPASS( 13, 1, lb, 0x000000000000000f, 1, tdat3 );
  TEST_LD_DEST_BYPASS( 14, 2, lb, 0x0000000000000000, 1, tdat1 );

  TEST_LD_SRC1_BYPASS( 15, 0, lb, 0xfffffffffffffff0, 1, tdat2 );
  TEST_LD_SRC1_BYPASS( 16, 1, lb, 0x000000000000000f, 1, tdat3 );
  TEST_LD_SRC1_BYPASS( 17, 2, lb, 0x0000000000000000, 1, tdat1 );

  #-------------------------------------------------------------
  # Test write-after-write hazard
  #-------------------------------------------------------------

  TEST_CASE( 18, x2, 2, \
    la  x5, tdat; \
    lb  x2, 0(x5); \
    li  x2, 2; \
  )

  TEST_CASE( 19, x2, 2, \
    la  x5, tdat; \
    lb  x2, 0(x5); \
    nop; \
    li  x2, 2; \
  )

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END


.data
tdat:
tdat1:  .byte 0xff
tdat2:  .byte 0x00
tdat3:  .byte 0xf0
tdat4:  .byte 0x0f

RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

