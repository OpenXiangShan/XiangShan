# See LICENSE for license details.

#*****************************************************************************
# bne.S
#-----------------------------------------------------------------------------
#
# Test bne instruction.
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

  TEST_BR2_OP_TAKEN( 2, bne,  0,  1 );
  TEST_BR2_OP_TAKEN( 3, bne,  1,  0 );
  TEST_BR2_OP_TAKEN( 4, bne, -1,  1 );
  TEST_BR2_OP_TAKEN( 5, bne,  1, -1 );

  TEST_BR2_OP_NOTTAKEN( 6, bne,  0,  0 );
  TEST_BR2_OP_NOTTAKEN( 7, bne,  1,  1 );
  TEST_BR2_OP_NOTTAKEN( 8, bne, -1, -1 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_BR2_SRC12_BYPASS( 9,  0, 0, bne, 0, 0 );
  TEST_BR2_SRC12_BYPASS( 10, 0, 1, bne, 0, 0 );
  TEST_BR2_SRC12_BYPASS( 11, 0, 2, bne, 0, 0 );
  TEST_BR2_SRC12_BYPASS( 12, 1, 0, bne, 0, 0 );
  TEST_BR2_SRC12_BYPASS( 13, 1, 1, bne, 0, 0 );
  TEST_BR2_SRC12_BYPASS( 14, 2, 0, bne, 0, 0 );

  TEST_BR2_SRC12_BYPASS( 15, 0, 0, bne, 0, 0 );
  TEST_BR2_SRC12_BYPASS( 16, 0, 1, bne, 0, 0 );
  TEST_BR2_SRC12_BYPASS( 17, 0, 2, bne, 0, 0 );
  TEST_BR2_SRC12_BYPASS( 18, 1, 0, bne, 0, 0 );
  TEST_BR2_SRC12_BYPASS( 19, 1, 1, bne, 0, 0 );
  TEST_BR2_SRC12_BYPASS( 20, 2, 0, bne, 0, 0 );

  #-------------------------------------------------------------
  # Test delay slot instructions not executed nor bypassed
  #-------------------------------------------------------------

  TEST_CASE( 21, x1, 3, \
    li  x1, 1; \
    bne x1, x0, 1f; \
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

