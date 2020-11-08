# See LICENSE for license details.

#*****************************************************************************
# sllw.S
#-----------------------------------------------------------------------------
#
# Test sllw instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_RR_OP( 2,  sllw, 0x0000000000000001, 0x0000000000000001, 0  );
  TEST_RR_OP( 3,  sllw, 0x0000000000000002, 0x0000000000000001, 1  );
  TEST_RR_OP( 4,  sllw, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_OP( 5,  sllw, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_OP( 6,  sllw, 0xffffffff80000000, 0x0000000000000001, 31 );

  TEST_RR_OP( 7,  sllw, 0xffffffffffffffff, 0xffffffffffffffff, 0  );
  TEST_RR_OP( 8,  sllw, 0xfffffffffffffffe, 0xffffffffffffffff, 1  );
  TEST_RR_OP( 9,  sllw, 0xffffffffffffff80, 0xffffffffffffffff, 7  );
  TEST_RR_OP( 10, sllw, 0xffffffffffffc000, 0xffffffffffffffff, 14 );
  TEST_RR_OP( 11, sllw, 0xffffffff80000000, 0xffffffffffffffff, 31 );

  TEST_RR_OP( 12, sllw, 0x0000000021212121, 0x0000000021212121, 0  );
  TEST_RR_OP( 13, sllw, 0x0000000042424242, 0x0000000021212121, 1  );
  TEST_RR_OP( 14, sllw, 0xffffffff90909080, 0x0000000021212121, 7  );
  TEST_RR_OP( 15, sllw, 0x0000000048484000, 0x0000000021212121, 14 );
  TEST_RR_OP( 16, sllw, 0xffffffff80000000, 0x0000000021212121, 31 );

  # Verify that shifts only use bottom five bits

  TEST_RR_OP( 17, sllw, 0x0000000021212121, 0x0000000021212121, 0xffffffffffffffe0 );
  TEST_RR_OP( 18, sllw, 0x0000000042424242, 0x0000000021212121, 0xffffffffffffffe1 );
  TEST_RR_OP( 19, sllw, 0xffffffff90909080, 0x0000000021212121, 0xffffffffffffffe7 );
  TEST_RR_OP( 20, sllw, 0x0000000048484000, 0x0000000021212121, 0xffffffffffffffee );
  TEST_RR_OP( 21, sllw, 0xffffffff80000000, 0x0000000021212121, 0xffffffffffffffff );

  # Verify that shifts ignore top 32 (using true 64-bit values)

  TEST_RR_OP( 44, sllw, 0x0000000012345678, 0xffffffff12345678, 0 );
  TEST_RR_OP( 45, sllw, 0x0000000023456780, 0xffffffff12345678, 4 );
  TEST_RR_OP( 46, sllw, 0xffffffff92345678, 0x0000000092345678, 0 );
  TEST_RR_OP( 47, sllw, 0xffffffff93456780, 0x0000000099345678, 4 );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_RR_SRC1_EQ_DEST( 22, sllw, 0x00000080, 0x00000001, 7  );
  TEST_RR_SRC2_EQ_DEST( 23, sllw, 0x00004000, 0x00000001, 14 );
  TEST_RR_SRC12_EQ_DEST( 24, sllw, 24, 3 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_RR_DEST_BYPASS( 25, 0, sllw, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_DEST_BYPASS( 26, 1, sllw, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_DEST_BYPASS( 27, 2, sllw, 0xffffffff80000000, 0x0000000000000001, 31 );

  TEST_RR_SRC12_BYPASS( 28, 0, 0, sllw, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_SRC12_BYPASS( 29, 0, 1, sllw, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_SRC12_BYPASS( 30, 0, 2, sllw, 0xffffffff80000000, 0x0000000000000001, 31 );
  TEST_RR_SRC12_BYPASS( 31, 1, 0, sllw, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_SRC12_BYPASS( 32, 1, 1, sllw, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_SRC12_BYPASS( 33, 2, 0, sllw, 0xffffffff80000000, 0x0000000000000001, 31 );

  TEST_RR_SRC21_BYPASS( 34, 0, 0, sllw, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_SRC21_BYPASS( 35, 0, 1, sllw, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_SRC21_BYPASS( 36, 0, 2, sllw, 0xffffffff80000000, 0x0000000000000001, 31 );
  TEST_RR_SRC21_BYPASS( 37, 1, 0, sllw, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_SRC21_BYPASS( 38, 1, 1, sllw, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_SRC21_BYPASS( 39, 2, 0, sllw, 0xffffffff80000000, 0x0000000000000001, 31 );

  TEST_RR_ZEROSRC1( 40, sllw, 0, 15 );
  TEST_RR_ZEROSRC2( 41, sllw, 32, 32 );
  TEST_RR_ZEROSRC12( 42, sllw, 0 );
  TEST_RR_ZERODEST( 43, sllw, 1024, 2048 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
