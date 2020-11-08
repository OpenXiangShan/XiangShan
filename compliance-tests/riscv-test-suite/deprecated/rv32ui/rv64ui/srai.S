# See LICENSE for license details.

#*****************************************************************************
# srai.S
#-----------------------------------------------------------------------------
#
# Test srai instruction.
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

  TEST_IMM_OP( 2,  srai, 0xffffff8000000000, 0xffffff8000000000, 0  );
  TEST_IMM_OP( 3,  srai, 0xffffffffc0000000, 0xffffffff80000000, 1  );
  TEST_IMM_OP( 4,  srai, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_IMM_OP( 5,  srai, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_IMM_OP( 6,  srai, 0xffffffffffffffff, 0xffffffff80000001, 31 );

  TEST_IMM_OP( 7,  srai, 0x000000007fffffff, 0x000000007fffffff, 0  );
  TEST_IMM_OP( 8,  srai, 0x000000003fffffff, 0x000000007fffffff, 1  );
  TEST_IMM_OP( 9,  srai, 0x0000000000ffffff, 0x000000007fffffff, 7  );
  TEST_IMM_OP( 10, srai, 0x000000000001ffff, 0x000000007fffffff, 14 );
  TEST_IMM_OP( 11, srai, 0x0000000000000000, 0x000000007fffffff, 31 );

  TEST_IMM_OP( 12, srai, 0xffffffff81818181, 0xffffffff81818181, 0  );
  TEST_IMM_OP( 13, srai, 0xffffffffc0c0c0c0, 0xffffffff81818181, 1  );
  TEST_IMM_OP( 14, srai, 0xffffffffff030303, 0xffffffff81818181, 7  );
  TEST_IMM_OP( 15, srai, 0xfffffffffffe0606, 0xffffffff81818181, 14 );
  TEST_IMM_OP( 16, srai, 0xffffffffffffffff, 0xffffffff81818181, 31 );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_IMM_SRC1_EQ_DEST( 17, srai, 0xffffffffff000000, 0xffffffff80000000, 7 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_IMM_DEST_BYPASS( 18, 0, srai, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_IMM_DEST_BYPASS( 19, 1, srai, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_IMM_DEST_BYPASS( 20, 2, srai, 0xffffffffffffffff, 0xffffffff80000001, 31 );

  TEST_IMM_SRC1_BYPASS( 21, 0, srai, 0xffffffffff000000, 0xffffffff80000000, 7 );
  TEST_IMM_SRC1_BYPASS( 22, 1, srai, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_IMM_SRC1_BYPASS( 23, 2, srai, 0xffffffffffffffff, 0xffffffff80000001, 31 );

  TEST_IMM_ZEROSRC1( 24, srai, 0, 4 );
  TEST_IMM_ZERODEST( 25, srai, 33, 10 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END


