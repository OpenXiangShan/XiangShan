# See LICENSE for license details.

#*****************************************************************************
# sll.S
#-----------------------------------------------------------------------------
#
# Test sll instruction.
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

  TEST_RR_OP( 2,  sll, 0x0000000000000001, 0x0000000000000001, 0  );
  TEST_RR_OP( 3,  sll, 0x0000000000000002, 0x0000000000000001, 1  );
  TEST_RR_OP( 4,  sll, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_OP( 5,  sll, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_OP( 6,  sll, 0x0000000080000000, 0x0000000000000001, 31 );

  TEST_RR_OP( 7,  sll, 0xffffffffffffffff, 0xffffffffffffffff, 0  );
  TEST_RR_OP( 8,  sll, 0xfffffffffffffffe, 0xffffffffffffffff, 1  );
  TEST_RR_OP( 9,  sll, 0xffffffffffffff80, 0xffffffffffffffff, 7  );
  TEST_RR_OP( 10, sll, 0xffffffffffffc000, 0xffffffffffffffff, 14 );
  TEST_RR_OP( 11, sll, 0xffffffff80000000, 0xffffffffffffffff, 31 );

  TEST_RR_OP( 12, sll, 0x0000000021212121, 0x0000000021212121, 0  );
  TEST_RR_OP( 13, sll, 0x0000000042424242, 0x0000000021212121, 1  );
  TEST_RR_OP( 14, sll, 0x0000001090909080, 0x0000000021212121, 7  );
  TEST_RR_OP( 15, sll, 0x0000084848484000, 0x0000000021212121, 14 );
  TEST_RR_OP( 16, sll, 0x1090909080000000, 0x0000000021212121, 31 );

  # Verify that shifts only use bottom six bits

  TEST_RR_OP( 17, sll, 0x0000000021212121, 0x0000000021212121, 0xffffffffffffffc0 );
  TEST_RR_OP( 18, sll, 0x0000000042424242, 0x0000000021212121, 0xffffffffffffffc1 );
  TEST_RR_OP( 19, sll, 0x0000001090909080, 0x0000000021212121, 0xffffffffffffffc7 );
  TEST_RR_OP( 20, sll, 0x0000084848484000, 0x0000000021212121, 0xffffffffffffffce );

#if __riscv_xlen == 64
  TEST_RR_OP( 21, sll, 0x8000000000000000, 0x0000000021212121, 0xffffffffffffffff );
  TEST_RR_OP( 50, sll, 0x8000000000000000, 0x0000000000000001, 63 );
  TEST_RR_OP( 51, sll, 0xffffff8000000000, 0xffffffffffffffff, 39 );
  TEST_RR_OP( 52, sll, 0x0909080000000000, 0x0000000021212121, 43 );
#endif

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_RR_SRC1_EQ_DEST( 22, sll, 0x00000080, 0x00000001, 7  );
  TEST_RR_SRC2_EQ_DEST( 23, sll, 0x00004000, 0x00000001, 14 );
  TEST_RR_SRC12_EQ_DEST( 24, sll, 24, 3 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_RR_DEST_BYPASS( 25, 0, sll, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_DEST_BYPASS( 26, 1, sll, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_DEST_BYPASS( 27, 2, sll, 0x0000000080000000, 0x0000000000000001, 31 );

  TEST_RR_SRC12_BYPASS( 28, 0, 0, sll, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_SRC12_BYPASS( 29, 0, 1, sll, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_SRC12_BYPASS( 30, 0, 2, sll, 0x0000000080000000, 0x0000000000000001, 31 );
  TEST_RR_SRC12_BYPASS( 31, 1, 0, sll, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_SRC12_BYPASS( 32, 1, 1, sll, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_SRC12_BYPASS( 33, 2, 0, sll, 0x0000000080000000, 0x0000000000000001, 31 );

  TEST_RR_SRC21_BYPASS( 34, 0, 0, sll, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_SRC21_BYPASS( 35, 0, 1, sll, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_SRC21_BYPASS( 36, 0, 2, sll, 0x0000000080000000, 0x0000000000000001, 31 );
  TEST_RR_SRC21_BYPASS( 37, 1, 0, sll, 0x0000000000000080, 0x0000000000000001, 7  );
  TEST_RR_SRC21_BYPASS( 38, 1, 1, sll, 0x0000000000004000, 0x0000000000000001, 14 );
  TEST_RR_SRC21_BYPASS( 39, 2, 0, sll, 0x0000000080000000, 0x0000000000000001, 31 );

  TEST_RR_ZEROSRC1( 40, sll, 0, 15 );
  TEST_RR_ZEROSRC2( 41, sll, 32, 32 );
  TEST_RR_ZEROSRC12( 42, sll, 0 );
  TEST_RR_ZERODEST( 43, sll, 1024, 2048 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

.data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

