# See LICENSE for license details.

#*****************************************************************************
# ori.S
#-----------------------------------------------------------------------------
#
# Test ori instruction.
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

  TEST_IMM_OP( 2, ori, 0xffffffffffffff0f, 0xffffffffff00ff00, 0xf0f );
  TEST_IMM_OP( 3, ori, 0x000000000ff00ff0, 0x000000000ff00ff0, 0x0f0 );
  TEST_IMM_OP( 4, ori, 0x0000000000ff07ff, 0x0000000000ff00ff, 0x70f );
  TEST_IMM_OP( 5, ori, 0xfffffffff00ff0ff, 0xfffffffff00ff00f, 0x0f0 );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_IMM_SRC1_EQ_DEST( 6, ori, 0xff00fff0, 0xff00ff00, 0x0f0 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_IMM_DEST_BYPASS( 7,  0, ori, 0x000000000ff00ff0, 0x000000000ff00ff0, 0x0f0 );
  TEST_IMM_DEST_BYPASS( 8,  1, ori, 0x0000000000ff07ff, 0x0000000000ff00ff, 0x70f );
  TEST_IMM_DEST_BYPASS( 9,  2, ori, 0xfffffffff00ff0ff, 0xfffffffff00ff00f, 0x0f0 );

  TEST_IMM_SRC1_BYPASS( 10, 0, ori, 0x000000000ff00ff0, 0x000000000ff00ff0, 0x0f0 );
  TEST_IMM_SRC1_BYPASS( 11, 1, ori, 0xffffffffffffffff, 0x0000000000ff00ff, 0xf0f );
  TEST_IMM_SRC1_BYPASS( 12, 2, ori, 0xfffffffff00ff0ff, 0xfffffffff00ff00f, 0x0f0 );

  TEST_IMM_ZEROSRC1( 13, ori, 0x0f0, 0x0f0 );
  TEST_IMM_ZERODEST( 14, ori, 0x00ff00ff, 0x70f );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

