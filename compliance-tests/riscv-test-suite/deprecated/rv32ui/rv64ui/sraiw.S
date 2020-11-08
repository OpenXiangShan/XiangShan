# See LICENSE for license details.

#*****************************************************************************
# sraiw.S
#-----------------------------------------------------------------------------
#
# Test sraiw instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_IMM_OP( 2,  sraiw, 0xffffffff80000000, 0xffffffff80000000, 0  );
  TEST_IMM_OP( 3,  sraiw, 0xffffffffc0000000, 0xffffffff80000000, 1  );
  TEST_IMM_OP( 4,  sraiw, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_IMM_OP( 5,  sraiw, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_IMM_OP( 6,  sraiw, 0xffffffffffffffff, 0xffffffff80000001, 31 );

  TEST_IMM_OP( 7,  sraiw, 0x000000007fffffff, 0x000000007fffffff, 0  );
  TEST_IMM_OP( 8,  sraiw, 0x000000003fffffff, 0x000000007fffffff, 1  );
  TEST_IMM_OP( 9,  sraiw, 0x0000000000ffffff, 0x000000007fffffff, 7  );
  TEST_IMM_OP( 10, sraiw, 0x000000000001ffff, 0x000000007fffffff, 14 );
  TEST_IMM_OP( 11, sraiw, 0x0000000000000000, 0x000000007fffffff, 31 );

  TEST_IMM_OP( 12, sraiw, 0xffffffff81818181, 0xffffffff81818181, 0  );
  TEST_IMM_OP( 13, sraiw, 0xffffffffc0c0c0c0, 0xffffffff81818181, 1  );
  TEST_IMM_OP( 14, sraiw, 0xffffffffff030303, 0xffffffff81818181, 7  );
  TEST_IMM_OP( 15, sraiw, 0xfffffffffffe0606, 0xffffffff81818181, 14 );
  TEST_IMM_OP( 16, sraiw, 0xffffffffffffffff, 0xffffffff81818181, 31 );

  # Verify that shifts ignore top 32 (using true 64-bit values)

  TEST_IMM_OP( 44, sraiw, 0x0000000012345678, 0xffffffff12345678, 0 );
  TEST_IMM_OP( 45, sraiw, 0x0000000001234567, 0xffffffff12345678, 4 );
  TEST_IMM_OP( 46, sraiw, 0xffffffff92345678, 0x0000000092345678, 0 );
  TEST_IMM_OP( 47, sraiw, 0xfffffffff9234567, 0x0000000092345678, 4 );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_IMM_SRC1_EQ_DEST( 17, sraiw, 0xffffffffff000000, 0xffffffff80000000, 7 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_IMM_DEST_BYPASS( 18, 0, sraiw, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_IMM_DEST_BYPASS( 19, 1, sraiw, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_IMM_DEST_BYPASS( 20, 2, sraiw, 0xffffffffffffffff, 0xffffffff80000001, 31 );

  TEST_IMM_SRC1_BYPASS( 21, 0, sraiw, 0xffffffffff000000, 0xffffffff80000000, 7 );
  TEST_IMM_SRC1_BYPASS( 22, 1, sraiw, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_IMM_SRC1_BYPASS( 23, 2, sraiw, 0xffffffffffffffff, 0xffffffff80000001, 31 );

  TEST_IMM_ZEROSRC1( 24, sraiw, 0, 31 );
  TEST_IMM_ZERODEST( 25, sraiw, 31, 28 );

  TEST_IMM_OP( 26, sraiw, 0x0000000000000000, 0x00e0000000000000, 28)
  TEST_IMM_OP( 27, sraiw, 0xffffffffff000000, 0x00000000f0000000, 4)

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
