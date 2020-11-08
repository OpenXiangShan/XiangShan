# See LICENSE for license details.

#*****************************************************************************
# and.S
#-----------------------------------------------------------------------------
#
# Test and instruction.
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

  TEST_RR_OP( 2, and, 0x0f000f00, 0xff00ff00, 0x0f0f0f0f );
  TEST_RR_OP( 3, and, 0x00f000f0, 0x0ff00ff0, 0xf0f0f0f0 );
  TEST_RR_OP( 4, and, 0x000f000f, 0x00ff00ff, 0x0f0f0f0f );
  TEST_RR_OP( 5, and, 0xf000f000, 0xf00ff00f, 0xf0f0f0f0 );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_RR_SRC1_EQ_DEST( 6, and, 0x0f000f00, 0xff00ff00, 0x0f0f0f0f );
  TEST_RR_SRC2_EQ_DEST( 7, and, 0x00f000f0, 0x0ff00ff0, 0xf0f0f0f0 );
  TEST_RR_SRC12_EQ_DEST( 8, and, 0xff00ff00, 0xff00ff00 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_RR_DEST_BYPASS( 9,  0, and, 0x0f000f00, 0xff00ff00, 0x0f0f0f0f );
  TEST_RR_DEST_BYPASS( 10, 1, and, 0x00f000f0, 0x0ff00ff0, 0xf0f0f0f0 );
  TEST_RR_DEST_BYPASS( 11, 2, and, 0x000f000f, 0x00ff00ff, 0x0f0f0f0f );

  TEST_RR_SRC12_BYPASS( 12, 0, 0, and, 0x0f000f00, 0xff00ff00, 0x0f0f0f0f );
  TEST_RR_SRC12_BYPASS( 13, 0, 1, and, 0x00f000f0, 0x0ff00ff0, 0xf0f0f0f0 );
  TEST_RR_SRC12_BYPASS( 14, 0, 2, and, 0x000f000f, 0x00ff00ff, 0x0f0f0f0f );
  TEST_RR_SRC12_BYPASS( 15, 1, 0, and, 0x0f000f00, 0xff00ff00, 0x0f0f0f0f );
  TEST_RR_SRC12_BYPASS( 16, 1, 1, and, 0x00f000f0, 0x0ff00ff0, 0xf0f0f0f0 );
  TEST_RR_SRC12_BYPASS( 17, 2, 0, and, 0x000f000f, 0x00ff00ff, 0x0f0f0f0f );

  TEST_RR_SRC21_BYPASS( 18, 0, 0, and, 0x0f000f00, 0xff00ff00, 0x0f0f0f0f );
  TEST_RR_SRC21_BYPASS( 19, 0, 1, and, 0x00f000f0, 0x0ff00ff0, 0xf0f0f0f0 );
  TEST_RR_SRC21_BYPASS( 20, 0, 2, and, 0x000f000f, 0x00ff00ff, 0x0f0f0f0f );
  TEST_RR_SRC21_BYPASS( 21, 1, 0, and, 0x0f000f00, 0xff00ff00, 0x0f0f0f0f );
  TEST_RR_SRC21_BYPASS( 22, 1, 1, and, 0x00f000f0, 0x0ff00ff0, 0xf0f0f0f0 );
  TEST_RR_SRC21_BYPASS( 23, 2, 0, and, 0x000f000f, 0x00ff00ff, 0x0f0f0f0f );

  TEST_RR_ZEROSRC1( 24, and, 0, 0xff00ff00 );
  TEST_RR_ZEROSRC2( 25, and, 0, 0x00ff00ff );
  TEST_RR_ZEROSRC12( 26, and, 0 );
  TEST_RR_ZERODEST( 27, and, 0x11111111, 0x22222222 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

