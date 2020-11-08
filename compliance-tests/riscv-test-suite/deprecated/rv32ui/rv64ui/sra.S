# See LICENSE for license details.

#*****************************************************************************
# sra.S
#-----------------------------------------------------------------------------
#
# Test sra instruction.
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

  TEST_RR_OP( 2,  sra, 0xffffffff80000000, 0xffffffff80000000, 0  );
  TEST_RR_OP( 3,  sra, 0xffffffffc0000000, 0xffffffff80000000, 1  );
  TEST_RR_OP( 4,  sra, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_OP( 5,  sra, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_OP( 6,  sra, 0xffffffffffffffff, 0xffffffff80000001, 31 );

  TEST_RR_OP( 7,  sra, 0x000000007fffffff, 0x000000007fffffff, 0  );
  TEST_RR_OP( 8,  sra, 0x000000003fffffff, 0x000000007fffffff, 1  );
  TEST_RR_OP( 9,  sra, 0x0000000000ffffff, 0x000000007fffffff, 7  );
  TEST_RR_OP( 10, sra, 0x000000000001ffff, 0x000000007fffffff, 14 );
  TEST_RR_OP( 11, sra, 0x0000000000000000, 0x000000007fffffff, 31 );

  TEST_RR_OP( 12, sra, 0xffffffff81818181, 0xffffffff81818181, 0  );
  TEST_RR_OP( 13, sra, 0xffffffffc0c0c0c0, 0xffffffff81818181, 1  );
  TEST_RR_OP( 14, sra, 0xffffffffff030303, 0xffffffff81818181, 7  );
  TEST_RR_OP( 15, sra, 0xfffffffffffe0606, 0xffffffff81818181, 14 );
  TEST_RR_OP( 16, sra, 0xffffffffffffffff, 0xffffffff81818181, 31 );

  # Verify that shifts only use bottom five bits

  TEST_RR_OP( 17, sra, 0xffffffff81818181, 0xffffffff81818181, 0xffffffffffffffc0 );
  TEST_RR_OP( 18, sra, 0xffffffffc0c0c0c0, 0xffffffff81818181, 0xffffffffffffffc1 );
  TEST_RR_OP( 19, sra, 0xffffffffff030303, 0xffffffff81818181, 0xffffffffffffffc7 );
  TEST_RR_OP( 20, sra, 0xfffffffffffe0606, 0xffffffff81818181, 0xffffffffffffffce );
  TEST_RR_OP( 21, sra, 0xffffffffffffffff, 0xffffffff81818181, 0xffffffffffffffff );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_RR_SRC1_EQ_DEST( 22, sra, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC2_EQ_DEST( 23, sra, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_SRC12_EQ_DEST( 24, sra, 0, 7 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_RR_DEST_BYPASS( 25, 0, sra, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_DEST_BYPASS( 26, 1, sra, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_DEST_BYPASS( 27, 2, sra, 0xffffffffffffffff, 0xffffffff80000000, 31 );

  TEST_RR_SRC12_BYPASS( 28, 0, 0, sra, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC12_BYPASS( 29, 0, 1, sra, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_SRC12_BYPASS( 30, 0, 2, sra, 0xffffffffffffffff, 0xffffffff80000000, 31 );
  TEST_RR_SRC12_BYPASS( 31, 1, 0, sra, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC12_BYPASS( 32, 1, 1, sra, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_SRC12_BYPASS( 33, 2, 0, sra, 0xffffffffffffffff, 0xffffffff80000000, 31 );

  TEST_RR_SRC21_BYPASS( 34, 0, 0, sra, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC21_BYPASS( 35, 0, 1, sra, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_SRC21_BYPASS( 36, 0, 2, sra, 0xffffffffffffffff, 0xffffffff80000000, 31 );
  TEST_RR_SRC21_BYPASS( 37, 1, 0, sra, 0xffffffffff000000, 0xffffffff80000000, 7  );
  TEST_RR_SRC21_BYPASS( 38, 1, 1, sra, 0xfffffffffffe0000, 0xffffffff80000000, 14 );
  TEST_RR_SRC21_BYPASS( 39, 2, 0, sra, 0xffffffffffffffff, 0xffffffff80000000, 31 );

  TEST_RR_ZEROSRC1( 40, sra, 0, 15 );
  TEST_RR_ZEROSRC2( 41, sra, 32, 32 );
  TEST_RR_ZEROSRC12( 42, sra, 0 );
  TEST_RR_ZERODEST( 43, sra, 1024, 2048 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 50, 4, -1
RV_COMPLIANCE_DATA_END

