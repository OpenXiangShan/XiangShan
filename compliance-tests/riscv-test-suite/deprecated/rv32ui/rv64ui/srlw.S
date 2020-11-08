# See LICENSE for license details.

#*****************************************************************************
# srlw.S
#-----------------------------------------------------------------------------
#
# Test srlw instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_RR_OP( 2,  srlw, 0xffffffff80000000, 0xffffffff80000000, 0  );
  TEST_RR_OP( 3,  srlw, 0x0000000040000000, 0xffffffff80000000, 1  );
  TEST_RR_OP( 4,  srlw, 0x0000000001000000, 0xffffffff80000000, 7  );
  TEST_RR_OP( 5,  srlw, 0x0000000000020000, 0xffffffff80000000, 14 );
  TEST_RR_OP( 6,  srlw, 0x0000000000000001, 0xffffffff80000001, 31 );

  TEST_RR_OP( 7,  srlw, 0xffffffffffffffff, 0xffffffffffffffff, 0  );
  TEST_RR_OP( 8,  srlw, 0x000000007fffffff, 0xffffffffffffffff, 1  );
  TEST_RR_OP( 9,  srlw, 0x0000000001ffffff, 0xffffffffffffffff, 7  );
  TEST_RR_OP( 10, srlw, 0x000000000003ffff, 0xffffffffffffffff, 14 );
  TEST_RR_OP( 11, srlw, 0x0000000000000001, 0xffffffffffffffff, 31 );

  TEST_RR_OP( 12, srlw, 0x0000000021212121, 0x0000000021212121, 0  );
  TEST_RR_OP( 13, srlw, 0x0000000010909090, 0x0000000021212121, 1  );
  TEST_RR_OP( 14, srlw, 0x0000000000424242, 0x0000000021212121, 7  );
  TEST_RR_OP( 15, srlw, 0x0000000000008484, 0x0000000021212121, 14 );
  TEST_RR_OP( 16, srlw, 0x0000000000000000, 0x0000000021212121, 31 );

  # Verify that shifts only use bottom five bits

  TEST_RR_OP( 17, srlw, 0x0000000021212121, 0x0000000021212121, 0xffffffffffffffe0 );
  TEST_RR_OP( 18, srlw, 0x0000000010909090, 0x0000000021212121, 0xffffffffffffffe1 );
  TEST_RR_OP( 19, srlw, 0x0000000000424242, 0x0000000021212121, 0xffffffffffffffe7 );
  TEST_RR_OP( 20, srlw, 0x0000000000008484, 0x0000000021212121, 0xffffffffffffffee );
  TEST_RR_OP( 21, srlw, 0x0000000000000000, 0x0000000021212121, 0xffffffffffffffff );

  # Verify that shifts ignore top 32 (using true 64-bit values)

  TEST_RR_OP( 44, srlw, 0x0000000012345678, 0xffffffff12345678, 0 );
  TEST_RR_OP( 45, srlw, 0x0000000001234567, 0xffffffff12345678, 4 );
  TEST_RR_OP( 46, srlw, 0xffffffff92345678, 0x0000000092345678, 0 );
  TEST_RR_OP( 47, srlw, 0x0000000009234567, 0x0000000092345678, 4 );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_RR_SRC1_EQ_DEST( 22, srlw, 0x0000000001000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC2_EQ_DEST( 23, srlw, 0x0000000000020000, 0xffffffff80000000, 14 );
  TEST_RR_SRC12_EQ_DEST( 24, srlw, 0, 7 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_RR_DEST_BYPASS( 25, 0, srlw, 0x0000000001000000, 0xffffffff80000000, 7  );
  TEST_RR_DEST_BYPASS( 26, 1, srlw, 0x0000000000020000, 0xffffffff80000000, 14 );
  TEST_RR_DEST_BYPASS( 27, 2, srlw, 0x0000000000000001, 0xffffffff80000000, 31 );

  TEST_RR_SRC12_BYPASS( 28, 0, 0, srlw, 0x0000000001000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC12_BYPASS( 29, 0, 1, srlw, 0x0000000000020000, 0xffffffff80000000, 14 );
  TEST_RR_SRC12_BYPASS( 30, 0, 2, srlw, 0x0000000000000001, 0xffffffff80000000, 31 );
  TEST_RR_SRC12_BYPASS( 31, 1, 0, srlw, 0x0000000001000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC12_BYPASS( 32, 1, 1, srlw, 0x0000000000020000, 0xffffffff80000000, 14 );
  TEST_RR_SRC12_BYPASS( 33, 2, 0, srlw, 0x0000000000000001, 0xffffffff80000000, 31 );

  TEST_RR_SRC21_BYPASS( 34, 0, 0, srlw, 0x0000000001000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC21_BYPASS( 35, 0, 1, srlw, 0x0000000000020000, 0xffffffff80000000, 14 );
  TEST_RR_SRC21_BYPASS( 36, 0, 2, srlw, 0x0000000000000001, 0xffffffff80000000, 31 );
  TEST_RR_SRC21_BYPASS( 37, 1, 0, srlw, 0x0000000001000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC21_BYPASS( 38, 1, 1, srlw, 0x0000000000020000, 0xffffffff80000000, 14 );
  TEST_RR_SRC21_BYPASS( 39, 2, 0, srlw, 0x0000000000000001, 0xffffffff80000000, 31 );

  TEST_RR_ZEROSRC1( 40, srlw, 0, 15 );
  TEST_RR_ZEROSRC2( 41, srlw, 32, 32 );
  TEST_RR_ZEROSRC12( 42, srlw, 0 );
  TEST_RR_ZERODEST( 43, srlw, 1024, 2048 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
