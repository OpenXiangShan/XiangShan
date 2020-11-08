# See LICENSE for license details.

#*****************************************************************************
# bltu.S
#-----------------------------------------------------------------------------
#
# Test bltu instruction.
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

  TEST_BR2_OP_TAKEN( 2, bltu, 0x00000000, 0x00000001 );
  TEST_BR2_OP_TAKEN( 3, bltu, 0xfffffffe, 0xffffffff );
  TEST_BR2_OP_TAKEN( 4, bltu, 0x00000000, 0xffffffff );

  TEST_BR2_OP_NOTTAKEN( 5, bltu, 0x00000001, 0x00000000 );
  TEST_BR2_OP_NOTTAKEN( 6, bltu, 0xffffffff, 0xfffffffe );
  TEST_BR2_OP_NOTTAKEN( 7, bltu, 0xffffffff, 0x00000000 );
  TEST_BR2_OP_NOTTAKEN( 8, bltu, 0x80000000, 0x7fffffff );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_BR2_SRC12_BYPASS( 9,  0, 0, bltu, 0xf0000000, 0xefffffff );
  TEST_BR2_SRC12_BYPASS( 10, 0, 1, bltu, 0xf0000000, 0xefffffff );
  TEST_BR2_SRC12_BYPASS( 11, 0, 2, bltu, 0xf0000000, 0xefffffff );
  TEST_BR2_SRC12_BYPASS( 12, 1, 0, bltu, 0xf0000000, 0xefffffff );
  TEST_BR2_SRC12_BYPASS( 13, 1, 1, bltu, 0xf0000000, 0xefffffff );
  TEST_BR2_SRC12_BYPASS( 14, 2, 0, bltu, 0xf0000000, 0xefffffff );

  TEST_BR2_SRC12_BYPASS( 15, 0, 0, bltu, 0xf0000000, 0xefffffff );
  TEST_BR2_SRC12_BYPASS( 16, 0, 1, bltu, 0xf0000000, 0xefffffff );
  TEST_BR2_SRC12_BYPASS( 17, 0, 2, bltu, 0xf0000000, 0xefffffff );
  TEST_BR2_SRC12_BYPASS( 18, 1, 0, bltu, 0xf0000000, 0xefffffff );
  TEST_BR2_SRC12_BYPASS( 19, 1, 1, bltu, 0xf0000000, 0xefffffff );
  TEST_BR2_SRC12_BYPASS( 20, 2, 0, bltu, 0xf0000000, 0xefffffff );

  #-------------------------------------------------------------
  # Test delay slot instructions not executed nor bypassed
  #-------------------------------------------------------------

  TEST_CASE( 21, x1, 3, \
    li  x1, 1; \
    bltu x0, x1, 1f; \
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

