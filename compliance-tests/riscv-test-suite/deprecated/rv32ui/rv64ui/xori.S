# See LICENSE for license details.

#*****************************************************************************
# xori.S
#-----------------------------------------------------------------------------
#
# Test xori instruction.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Logical tests
  #-------------------------------------------------------------

  TEST_IMM_OP( 2, xori, 0xffffffffff00f00f, 0x0000000000ff0f00, 0xf0f );
  TEST_IMM_OP( 3, xori, 0x000000000ff00f00, 0x000000000ff00ff0, 0x0f0 );
  TEST_IMM_OP( 4, xori, 0x0000000000ff0ff0, 0x0000000000ff08ff, 0x70f );
  TEST_IMM_OP( 5, xori, 0xfffffffff00ff0ff, 0xfffffffff00ff00f, 0x0f0 );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_IMM_SRC1_EQ_DEST( 6, xori, 0xffffffffff00f00f, 0xffffffffff00f700, 0x70f );

   #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_IMM_DEST_BYPASS( 7,  0, xori, 0x000000000ff00f00, 0x000000000ff00ff0, 0x0f0 );
  TEST_IMM_DEST_BYPASS( 8,  1, xori, 0x0000000000ff0ff0, 0x0000000000ff08ff, 0x70f );
  TEST_IMM_DEST_BYPASS( 9,  2, xori, 0xfffffffff00ff0ff, 0xfffffffff00ff00f, 0x0f0 );

  TEST_IMM_SRC1_BYPASS( 10, 0, xori, 0x000000000ff00f00, 0x000000000ff00ff0, 0x0f0 );
  TEST_IMM_SRC1_BYPASS( 11, 1, xori, 0x0000000000ff0ff0, 0x0000000000ff0fff, 0x00f );
  TEST_IMM_SRC1_BYPASS( 12, 2, xori, 0xfffffffff00ff0ff, 0xfffffffff00ff00f, 0x0f0 );

  TEST_IMM_ZEROSRC1( 13, xori, 0x0f0, 0x0f0 );
  TEST_IMM_ZERODEST( 14, xori, 0x00ff00ff, 0x70f );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END


