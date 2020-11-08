# See LICENSE for license details.

#*****************************************************************************
# addiw.S
#-----------------------------------------------------------------------------
#
# Test addiw instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_IMM_OP( 2,  addiw, 0x00000000, 0x00000000, 0x000 );
  TEST_IMM_OP( 3,  addiw, 0x00000002, 0x00000001, 0x001 );
  TEST_IMM_OP( 4,  addiw, 0x0000000a, 0x00000003, 0x007 );

  TEST_IMM_OP( 5,  addiw, 0xfffffffffffff800, 0x0000000000000000, 0x800 );
  TEST_IMM_OP( 6,  addiw, 0xffffffff80000000, 0xffffffff80000000, 0x000 );
  TEST_IMM_OP( 7,  addiw, 0x000000007ffff800, 0xffffffff80000000, 0x800 );

  TEST_IMM_OP( 8,  addiw, 0x00000000000007ff, 0x00000000, 0x7ff );
  TEST_IMM_OP( 9,  addiw, 0x000000007fffffff, 0x7fffffff, 0x000 );
  TEST_IMM_OP( 10, addiw, 0xffffffff800007fe, 0x7fffffff, 0x7ff );

  TEST_IMM_OP( 11, addiw, 0xffffffff800007ff, 0xffffffff80000000, 0x7ff );
  TEST_IMM_OP( 12, addiw, 0x000000007ffff7ff, 0x000000007fffffff, 0x800 );

  TEST_IMM_OP( 13, addiw, 0xffffffffffffffff, 0x0000000000000000, 0xfff );
  TEST_IMM_OP( 14, addiw, 0x0000000000000000, 0xffffffffffffffff, 0x001 );
  TEST_IMM_OP( 15, addiw, 0xfffffffffffffffe, 0xffffffffffffffff, 0xfff );

  TEST_IMM_OP( 16, addiw, 0xffffffff80000000, 0x7fffffff, 0x001 );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_IMM_SRC1_EQ_DEST( 17, addiw, 24, 13, 11 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_IMM_DEST_BYPASS( 18, 0, addiw, 24, 13, 11 );
  TEST_IMM_DEST_BYPASS( 19, 1, addiw, 23, 13, 10 );
  TEST_IMM_DEST_BYPASS( 20, 2, addiw, 22, 13,  9 );

  TEST_IMM_SRC1_BYPASS( 21, 0, addiw, 24, 13, 11 );
  TEST_IMM_SRC1_BYPASS( 22, 1, addiw, 23, 13, 10 );
  TEST_IMM_SRC1_BYPASS( 23, 2, addiw, 22, 13,  9 );

  TEST_IMM_ZEROSRC1( 24, addiw, 32, 32 );
  TEST_IMM_ZERODEST( 25, addiw, 33, 50 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
