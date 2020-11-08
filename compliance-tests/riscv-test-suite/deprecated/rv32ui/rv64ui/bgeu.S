# See LICENSE for license details.

#*****************************************************************************
# bgeu.S
#-----------------------------------------------------------------------------
#
# Test bgeu instruction.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Branch tests
  #-------------------------------------------------------------

  # Each test checks both forward and backward branches

  TEST_BR2_OP_TAKEN( 2, bgeu, 0x00000000, 0x00000000 );
  TEST_BR2_OP_TAKEN( 3, bgeu, 0x00000001, 0x00000001 );
  TEST_BR2_OP_TAKEN( 4, bgeu, 0xffffffff, 0xffffffff );
  TEST_BR2_OP_TAKEN( 5, bgeu, 0x00000001, 0x00000000 );
  TEST_BR2_OP_TAKEN( 6, bgeu, 0xffffffff, 0xfffffffe );
  TEST_BR2_OP_TAKEN( 7, bgeu, 0xffffffff, 0x00000000 );

  TEST_BR2_OP_NOTTAKEN(  8, bgeu, 0x00000000, 0x00000001 );
  TEST_BR2_OP_NOTTAKEN(  9, bgeu, 0xfffffffe, 0xffffffff );
  TEST_BR2_OP_NOTTAKEN( 10, bgeu, 0x00000000, 0xffffffff );
  TEST_BR2_OP_NOTTAKEN( 11, bgeu, 0x7fffffff, 0x80000000 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_BR2_SRC12_BYPASS( 12, 0, 0, bgeu, 0xefffffff, 0xf0000000 );
  TEST_BR2_SRC12_BYPASS( 13, 0, 1, bgeu, 0xefffffff, 0xf0000000 );
  TEST_BR2_SRC12_BYPASS( 14, 0, 2, bgeu, 0xefffffff, 0xf0000000 );
  TEST_BR2_SRC12_BYPASS( 15, 1, 0, bgeu, 0xefffffff, 0xf0000000 );
  TEST_BR2_SRC12_BYPASS( 16, 1, 1, bgeu, 0xefffffff, 0xf0000000 );
  TEST_BR2_SRC12_BYPASS( 17, 2, 0, bgeu, 0xefffffff, 0xf0000000 );

  TEST_BR2_SRC12_BYPASS( 18, 0, 0, bgeu, 0xefffffff, 0xf0000000 );
  TEST_BR2_SRC12_BYPASS( 19, 0, 1, bgeu, 0xefffffff, 0xf0000000 );
  TEST_BR2_SRC12_BYPASS( 20, 0, 2, bgeu, 0xefffffff, 0xf0000000 );
  TEST_BR2_SRC12_BYPASS( 21, 1, 0, bgeu, 0xefffffff, 0xf0000000 );
  TEST_BR2_SRC12_BYPASS( 22, 1, 1, bgeu, 0xefffffff, 0xf0000000 );
  TEST_BR2_SRC12_BYPASS( 23, 2, 0, bgeu, 0xefffffff, 0xf0000000 );

  #-------------------------------------------------------------
  # Test delay slot instructions not executed nor bypassed
  #-------------------------------------------------------------

  TEST_CASE( 24, x1, 3, \
    li  x1, 1; \
    bgeu x1, x0, 1f; \
    addi x1, x1, 1; \
    addi x1, x1, 1; \
    addi x1, x1, 1; \
    addi x1, x1, 1; \
1:  addi x1, x1, 1; \
    addi x1, x1, 1; \
  )

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 30, 4, -1
RV_COMPLIANCE_DATA_END


