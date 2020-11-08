# See LICENSE for license details.

#*****************************************************************************
# sraw.S
#-----------------------------------------------------------------------------
#
# Test sraw instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_RR_OP( 2,  sraw, 0xffffffff80000000, 0xffffffff80000000, 0  );
  TEST_RR_OP( 3,  sraw, 0xffffffffc0000000, 0xffffffff80000000, 1  );
  TEST_RR_OP( 4,  sraw, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_OP( 5,  sraw, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_OP( 6,  sraw, 0xffffffffffffffff, 0xffffffff80000001, 31 );

  TEST_RR_OP( 7,  sraw, 0x000000007fffffff, 0x000000007fffffff, 0  );
  TEST_RR_OP( 8,  sraw, 0x000000003fffffff, 0x000000007fffffff, 1  );
  TEST_RR_OP( 9,  sraw, 0x0000000000ffffff, 0x000000007fffffff, 7  );
  TEST_RR_OP( 10, sraw, 0x000000000001ffff, 0x000000007fffffff, 14 );
  TEST_RR_OP( 11, sraw, 0x0000000000000000, 0x000000007fffffff, 31 );

  TEST_RR_OP( 12, sraw, 0xffffffff81818181, 0xffffffff81818181, 0  );
  TEST_RR_OP( 13, sraw, 0xffffffffc0c0c0c0, 0xffffffff81818181, 1  );
  TEST_RR_OP( 14, sraw, 0xffffffffff030303, 0xffffffff81818181, 7  );
  TEST_RR_OP( 15, sraw, 0xfffffffffffe0606, 0xffffffff81818181, 14 );
  TEST_RR_OP( 16, sraw, 0xffffffffffffffff, 0xffffffff81818181, 31 );

  # Verify that shifts only use bottom five bits

  TEST_RR_OP( 17, sraw, 0xffffffff81818181, 0xffffffff81818181, 0xffffffffffffffe0 );
  TEST_RR_OP( 18, sraw, 0xffffffffc0c0c0c0, 0xffffffff81818181, 0xffffffffffffffe1 );
  TEST_RR_OP( 19, sraw, 0xffffffffff030303, 0xffffffff81818181, 0xffffffffffffffe7 );
  TEST_RR_OP( 20, sraw, 0xfffffffffffe0606, 0xffffffff81818181, 0xffffffffffffffee );
  TEST_RR_OP( 21, sraw, 0xffffffffffffffff, 0xffffffff81818181, 0xffffffffffffffff );

  # Verify that shifts ignore top 32 (using true 64-bit values)

  TEST_RR_OP( 44, sraw, 0x0000000012345678, 0xffffffff12345678, 0 );
  TEST_RR_OP( 45, sraw, 0x0000000001234567, 0xffffffff12345678, 4 );
  TEST_RR_OP( 46, sraw, 0xffffffff92345678, 0x0000000092345678, 0 );
  TEST_RR_OP( 47, sraw, 0xfffffffff9234567, 0x0000000092345678, 4 );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_RR_SRC1_EQ_DEST( 22, sraw, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC2_EQ_DEST( 23, sraw, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_SRC12_EQ_DEST( 24, sraw, 0, 7 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_RR_DEST_BYPASS( 25, 0, sraw, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_DEST_BYPASS( 26, 1, sraw, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_DEST_BYPASS( 27, 2, sraw, 0xffffffffffffffff, 0xffffffff80000000, 31 );

  TEST_RR_SRC12_BYPASS( 28, 0, 0, sraw, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC12_BYPASS( 29, 0, 1, sraw, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_SRC12_BYPASS( 30, 0, 2, sraw, 0xffffffffffffffff, 0xffffffff80000000, 31 );
  TEST_RR_SRC12_BYPASS( 31, 1, 0, sraw, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC12_BYPASS( 32, 1, 1, sraw, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_SRC12_BYPASS( 33, 2, 0, sraw, 0xffffffffffffffff, 0xffffffff80000000, 31 );

  TEST_RR_SRC21_BYPASS( 34, 0, 0, sraw, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC21_BYPASS( 35, 0, 1, sraw, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_SRC21_BYPASS( 36, 0, 2, sraw, 0xffffffffffffffff, 0xffffffff80000000, 31 );
  TEST_RR_SRC21_BYPASS( 37, 1, 0, sraw, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC21_BYPASS( 38, 1, 1, sraw, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_SRC21_BYPASS( 39, 2, 0, sraw, 0xffffffffffffffff, 0xffffffff80000000, 31 );

  TEST_RR_ZEROSRC1( 40, sraw, 0, 15 );
  TEST_RR_ZEROSRC2( 41, sraw, 32, 32 );
  TEST_RR_ZEROSRC12( 42, sraw, 0 );
  TEST_RR_ZERODEST( 43, sraw, 1024, 2048 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
