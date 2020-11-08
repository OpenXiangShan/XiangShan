# See LICENSE for license details.

#*****************************************************************************
# slti.S
#-----------------------------------------------------------------------------
#
# Test slti instruction.
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

  TEST_IMM_OP( 2,  slti, 0, 0x0000000000000000, 0x000 );
  TEST_IMM_OP( 3,  slti, 0, 0x0000000000000001, 0x001 );
  TEST_IMM_OP( 4,  slti, 1, 0x0000000000000003, 0x007 );
  TEST_IMM_OP( 5,  slti, 0, 0x0000000000000007, 0x003 );

  TEST_IMM_OP( 6,  slti, 0, 0x0000000000000000, 0x800 );
  TEST_IMM_OP( 7,  slti, 1, 0xffffffff80000000, 0x000 );
  TEST_IMM_OP( 8,  slti, 1, 0xffffffff80000000, 0x800 );

  TEST_IMM_OP( 9,  slti, 1, 0x0000000000000000, 0x7ff );
  TEST_IMM_OP( 10, slti, 0, 0x000000007fffffff, 0x000 );
  TEST_IMM_OP( 11, slti, 0, 0x000000007fffffff, 0x7ff );

  TEST_IMM_OP( 12, slti, 1, 0xffffffff80000000, 0x7ff );
  TEST_IMM_OP( 13, slti, 0, 0x000000007fffffff, 0x800 );

  TEST_IMM_OP( 14, slti, 0, 0x0000000000000000, 0xfff );
  TEST_IMM_OP( 15, slti, 1, 0xffffffffffffffff, 0x001 );
  TEST_IMM_OP( 16, slti, 0, 0xffffffffffffffff, 0xfff );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_IMM_SRC1_EQ_DEST( 17, slti, 1, 11, 13 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_IMM_DEST_BYPASS( 18, 0, slti, 0, 15, 10 );
  TEST_IMM_DEST_BYPASS( 19, 1, slti, 1, 10, 16 );
  TEST_IMM_DEST_BYPASS( 20, 2, slti, 0, 16,  9 );

  TEST_IMM_SRC1_BYPASS( 21, 0, slti, 1, 11, 15 );
  TEST_IMM_SRC1_BYPASS( 22, 1, slti, 0, 17,  8 );
  TEST_IMM_SRC1_BYPASS( 23, 2, slti, 1, 12, 14 );

  TEST_IMM_ZEROSRC1( 24, slti, 0, 0xfff );
  TEST_IMM_ZERODEST( 25, slti, 0x00ff00ff, 0xfff );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

