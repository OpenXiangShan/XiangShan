# See LICENSE for license details.

#*****************************************************************************
# add.S
#-----------------------------------------------------------------------------
#
# Test add instruction.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_RR_OP( 2,  add, 0x00000000, 0x00000000, 0x00000000 );
  TEST_RR_OP( 3,  add, 0x00000002, 0x00000001, 0x00000001 );
  TEST_RR_OP( 4,  add, 0x0000000a, 0x00000003, 0x00000007 );

  TEST_RR_OP( 5,  add, 0xffffffffffff8000, 0x0000000000000000, 0xffffffffffff8000 );
  TEST_RR_OP( 6,  add, 0xffffffff80000000, 0xffffffff80000000, 0x00000000 );
  TEST_RR_OP( 7,  add, 0xffffffff7fff8000, 0xffffffff80000000, 0xffffffffffff8000 );

  TEST_RR_OP( 8,  add, 0x0000000000007fff, 0x0000000000000000, 0x0000000000007fff );
  TEST_RR_OP( 9,  add, 0x000000007fffffff, 0x000000007fffffff, 0x0000000000000000 );
  TEST_RR_OP( 10, add, 0x0000000080007ffe, 0x000000007fffffff, 0x0000000000007fff );

  TEST_RR_OP( 11, add, 0xffffffff80007fff, 0xffffffff80000000, 0x0000000000007fff );
  TEST_RR_OP( 12, add, 0x000000007fff7fff, 0x000000007fffffff, 0xffffffffffff8000 );

  TEST_RR_OP( 13, add, 0xffffffffffffffff, 0x0000000000000000, 0xffffffffffffffff );
  TEST_RR_OP( 14, add, 0x0000000000000000, 0xffffffffffffffff, 0x0000000000000001 );
  TEST_RR_OP( 15, add, 0xfffffffffffffffe, 0xffffffffffffffff, 0xffffffffffffffff );

  TEST_RR_OP( 16, add, 0x0000000080000000, 0x0000000000000001, 0x000000007fffffff );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_RR_SRC1_EQ_DEST( 17, add, 24, 13, 11 );
  TEST_RR_SRC2_EQ_DEST( 18, add, 25, 14, 11 );
  TEST_RR_SRC12_EQ_DEST( 19, add, 26, 13 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_RR_DEST_BYPASS( 20, 0, add, 24, 13, 11 );
  TEST_RR_DEST_BYPASS( 21, 1, add, 25, 14, 11 );
  TEST_RR_DEST_BYPASS( 22, 2, add, 26, 15, 11 );

  TEST_RR_SRC12_BYPASS( 23, 0, 0, add, 24, 13, 11 );
  TEST_RR_SRC12_BYPASS( 24, 0, 1, add, 25, 14, 11 );
  TEST_RR_SRC12_BYPASS( 25, 0, 2, add, 26, 15, 11 );
  TEST_RR_SRC12_BYPASS( 26, 1, 0, add, 24, 13, 11 );
  TEST_RR_SRC12_BYPASS( 27, 1, 1, add, 25, 14, 11 );
  TEST_RR_SRC12_BYPASS( 28, 2, 0, add, 26, 15, 11 );

  TEST_RR_SRC21_BYPASS( 29, 0, 0, add, 24, 13, 11 );
  TEST_RR_SRC21_BYPASS( 30, 0, 1, add, 25, 14, 11 );
  TEST_RR_SRC21_BYPASS( 31, 0, 2, add, 26, 15, 11 );
  TEST_RR_SRC21_BYPASS( 32, 1, 0, add, 24, 13, 11 );
  TEST_RR_SRC21_BYPASS( 33, 1, 1, add, 25, 14, 11 );
  TEST_RR_SRC21_BYPASS( 34, 2, 0, add, 26, 15, 11 );

  TEST_RR_ZEROSRC1( 35, add, 15, 15 );
  TEST_RR_ZEROSRC2( 36, add, 32, 32 );
  TEST_RR_ZEROSRC12( 37, add, 0 );
  TEST_RR_ZERODEST( 38, add, 16, 30 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END


