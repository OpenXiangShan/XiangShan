# See LICENSE for license details.

#*****************************************************************************
# srliw.S
#-----------------------------------------------------------------------------
#
# Test srliw instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_IMM_OP( 2,  srliw, 0xffffffff80000000, 0xffffffff80000000, 0  );
  TEST_IMM_OP( 3,  srliw, 0x0000000040000000, 0xffffffff80000000, 1  );
  TEST_IMM_OP( 4,  srliw, 0x0000000001000000, 0xffffffff80000000, 7  );
  TEST_IMM_OP( 5,  srliw, 0x0000000000020000, 0xffffffff80000000, 14 );
  TEST_IMM_OP( 6,  srliw, 0x0000000000000001, 0xffffffff80000001, 31 );

  TEST_IMM_OP( 7,  srliw, 0xffffffffffffffff, 0xffffffffffffffff, 0  );
  TEST_IMM_OP( 8,  srliw, 0x000000007fffffff, 0xffffffffffffffff, 1  );
  TEST_IMM_OP( 9,  srliw, 0x0000000001ffffff, 0xffffffffffffffff, 7  );
  TEST_IMM_OP( 10, srliw, 0x000000000003ffff, 0xffffffffffffffff, 14 );
  TEST_IMM_OP( 11, srliw, 0x0000000000000001, 0xffffffffffffffff, 31 );

  TEST_IMM_OP( 12, srliw, 0x0000000021212121, 0x0000000021212121, 0  );
  TEST_IMM_OP( 13, srliw, 0x0000000010909090, 0x0000000021212121, 1  );
  TEST_IMM_OP( 14, srliw, 0x0000000000424242, 0x0000000021212121, 7  );
  TEST_IMM_OP( 15, srliw, 0x0000000000008484, 0x0000000021212121, 14 );
  TEST_IMM_OP( 16, srliw, 0x0000000000000000, 0x0000000021212121, 31 );

  # Verify that shifts ignore top 32 (using true 64-bit values)

  TEST_IMM_OP( 44, srliw, 0x0000000012345678, 0xffffffff12345678, 0 );
  TEST_IMM_OP( 45, srliw, 0x0000000001234567, 0xffffffff12345678, 4 );
  TEST_IMM_OP( 46, srliw, 0xffffffff92345678, 0x0000000092345678, 0 );
  TEST_IMM_OP( 47, srliw, 0x0000000009234567, 0x0000000092345678, 4 );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_IMM_SRC1_EQ_DEST( 17, srliw, 0x0000000001000000, 0xffffffff80000000, 7 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_IMM_DEST_BYPASS( 18, 0, srliw, 0x0000000001000000, 0xffffffff80000000, 7  );
  TEST_IMM_DEST_BYPASS( 19, 1, srliw, 0x0000000000020000, 0xffffffff80000000, 14 );
  TEST_IMM_DEST_BYPASS( 20, 2, srliw, 0x0000000000000001, 0xffffffff80000001, 31 );

  TEST_IMM_SRC1_BYPASS( 21, 0, srliw, 0x0000000001000000, 0xffffffff80000000, 7  );
  TEST_IMM_SRC1_BYPASS( 22, 1, srliw, 0x0000000000020000, 0xffffffff80000000, 14 );
  TEST_IMM_SRC1_BYPASS( 23, 2, srliw, 0x0000000000000001, 0xffffffff80000001, 31 );

  TEST_IMM_ZEROSRC1( 24, srliw, 0, 31 );
  TEST_IMM_ZERODEST( 25, srliw, 31, 28 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
