# See LICENSE for license details.

#*****************************************************************************
# subw.S
#-----------------------------------------------------------------------------
#
# Test subw instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_RR_OP( 2,  subw, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 );
  TEST_RR_OP( 3,  subw, 0x0000000000000000, 0x0000000000000001, 0x0000000000000001 );
  TEST_RR_OP( 4,  subw, 0xfffffffffffffffc, 0x0000000000000003, 0x0000000000000007 );

  TEST_RR_OP( 5,  subw, 0x0000000000008000, 0x0000000000000000, 0xffffffffffff8000 );
  TEST_RR_OP( 6,  subw, 0xffffffff80000000, 0xffffffff80000000, 0x0000000000000000 );
  TEST_RR_OP( 7,  subw, 0xffffffff80008000, 0xffffffff80000000, 0xffffffffffff8000 );

  TEST_RR_OP( 8,  subw, 0xffffffffffff8001, 0x0000000000000000, 0x0000000000007fff );
  TEST_RR_OP( 9,  subw, 0x000000007fffffff, 0x000000007fffffff, 0x0000000000000000 );
  TEST_RR_OP( 10, subw, 0x000000007fff8000, 0x000000007fffffff, 0x0000000000007fff );

  TEST_RR_OP( 11, subw, 0x000000007fff8001, 0xffffffff80000000, 0x0000000000007fff );
  TEST_RR_OP( 12, subw, 0xffffffff80007fff, 0x000000007fffffff, 0xffffffffffff8000 );

  TEST_RR_OP( 13, subw, 0x0000000000000001, 0x0000000000000000, 0xffffffffffffffff );
  TEST_RR_OP( 14, subw, 0xfffffffffffffffe, 0xffffffffffffffff, 0x0000000000000001 );
  TEST_RR_OP( 15, subw, 0x0000000000000000, 0xffffffffffffffff, 0xffffffffffffffff );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_RR_SRC1_EQ_DEST( 16, subw, 2, 13, 11 );
  TEST_RR_SRC2_EQ_DEST( 17, subw, 3, 14, 11 );
  TEST_RR_SRC12_EQ_DEST( 18, subw, 0, 13 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_RR_DEST_BYPASS( 19, 0, subw, 2, 13, 11 );
  TEST_RR_DEST_BYPASS( 20, 1, subw, 3, 14, 11 );
  TEST_RR_DEST_BYPASS( 21, 2, subw, 4, 15, 11 );

  TEST_RR_SRC12_BYPASS( 22, 0, 0, subw, 2, 13, 11 );
  TEST_RR_SRC12_BYPASS( 23, 0, 1, subw, 3, 14, 11 );
  TEST_RR_SRC12_BYPASS( 24, 0, 2, subw, 4, 15, 11 );
  TEST_RR_SRC12_BYPASS( 25, 1, 0, subw, 2, 13, 11 );
  TEST_RR_SRC12_BYPASS( 26, 1, 1, subw, 3, 14, 11 );
  TEST_RR_SRC12_BYPASS( 27, 2, 0, subw, 4, 15, 11 );

  TEST_RR_SRC21_BYPASS( 28, 0, 0, subw, 2, 13, 11 );
  TEST_RR_SRC21_BYPASS( 29, 0, 1, subw, 3, 14, 11 );
  TEST_RR_SRC21_BYPASS( 30, 0, 2, subw, 4, 15, 11 );
  TEST_RR_SRC21_BYPASS( 31, 1, 0, subw, 2, 13, 11 );
  TEST_RR_SRC21_BYPASS( 32, 1, 1, subw, 3, 14, 11 );
  TEST_RR_SRC21_BYPASS( 33, 2, 0, subw, 4, 15, 11 );

  TEST_RR_ZEROSRC1( 34, subw, 15, -15 );
  TEST_RR_ZEROSRC2( 35, subw, 32, 32 );
  TEST_RR_ZEROSRC12( 36, subw, 0 );
  TEST_RR_ZERODEST( 37, subw, 16, 30 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
