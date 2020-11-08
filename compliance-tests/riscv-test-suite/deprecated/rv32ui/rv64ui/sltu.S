# See LICENSE for license details.

#*****************************************************************************
# sltu.S
#-----------------------------------------------------------------------------
#
# Test sltu instruction.
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

  TEST_RR_OP( 2,  sltu, 0, 0x00000000, 0x00000000 );
  TEST_RR_OP( 3,  sltu, 0, 0x00000001, 0x00000001 );
  TEST_RR_OP( 4,  sltu, 1, 0x00000003, 0x00000007 );
  TEST_RR_OP( 5,  sltu, 0, 0x00000007, 0x00000003 );

  TEST_RR_OP( 6,  sltu, 1, 0x00000000, 0xffff8000 );
  TEST_RR_OP( 7,  sltu, 0, 0x80000000, 0x00000000 );
  TEST_RR_OP( 8,  sltu, 1, 0x80000000, 0xffff8000 );

  TEST_RR_OP( 9,  sltu, 1, 0x00000000, 0x00007fff );
  TEST_RR_OP( 10, sltu, 0, 0x7fffffff, 0x00000000 );
  TEST_RR_OP( 11, sltu, 0, 0x7fffffff, 0x00007fff );

  TEST_RR_OP( 12, sltu, 0, 0x80000000, 0x00007fff );
  TEST_RR_OP( 13, sltu, 1, 0x7fffffff, 0xffff8000 );

  TEST_RR_OP( 14, sltu, 1, 0x00000000, 0xffffffff );
  TEST_RR_OP( 15, sltu, 0, 0xffffffff, 0x00000001 );
  TEST_RR_OP( 16, sltu, 0, 0xffffffff, 0xffffffff );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_RR_SRC1_EQ_DEST( 17, sltu, 0, 14, 13 );
  TEST_RR_SRC2_EQ_DEST( 18, sltu, 1, 11, 13 );
  TEST_RR_SRC12_EQ_DEST( 19, sltu, 0, 13 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_RR_DEST_BYPASS( 20, 0, sltu, 1, 11, 13 );
  TEST_RR_DEST_BYPASS( 21, 1, sltu, 0, 14, 13 );
  TEST_RR_DEST_BYPASS( 22, 2, sltu, 1, 12, 13 );

  TEST_RR_SRC12_BYPASS( 23, 0, 0, sltu, 0, 14, 13 );
  TEST_RR_SRC12_BYPASS( 24, 0, 1, sltu, 1, 11, 13 );
  TEST_RR_SRC12_BYPASS( 25, 0, 2, sltu, 0, 15, 13 );
  TEST_RR_SRC12_BYPASS( 26, 1, 0, sltu, 1, 10, 13 );
  TEST_RR_SRC12_BYPASS( 27, 1, 1, sltu, 0, 16, 13 );
  TEST_RR_SRC12_BYPASS( 28, 2, 0, sltu, 1,  9, 13 );

  TEST_RR_SRC21_BYPASS( 29, 0, 0, sltu, 0, 17, 13 );
  TEST_RR_SRC21_BYPASS( 30, 0, 1, sltu, 1,  8, 13 );
  TEST_RR_SRC21_BYPASS( 31, 0, 2, sltu, 0, 18, 13 );
  TEST_RR_SRC21_BYPASS( 32, 1, 0, sltu, 1,  7, 13 );
  TEST_RR_SRC21_BYPASS( 33, 1, 1, sltu, 0, 19, 13 );
  TEST_RR_SRC21_BYPASS( 34, 2, 0, sltu, 1,  6, 13 );

  TEST_RR_ZEROSRC1( 35, sltu, 1, -1 );
  TEST_RR_ZEROSRC2( 36, sltu, 0, -1 );
  TEST_RR_ZEROSRC12( 37, sltu, 0 );
  TEST_RR_ZERODEST( 38, sltu, 16, 30 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END
  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

