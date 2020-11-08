# See LICENSE for license details.

#*****************************************************************************
# srl.S
#-----------------------------------------------------------------------------
#
# Test srl instruction.
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

#define TEST_SRL(n, v, a) \
  TEST_RR_OP(n, srl, ((v) & ((1 << (__riscv_xlen-1) << 1) - 1)) >> (a), v, a)

  TEST_SRL( 2,  0xffffffff80000000, 0  );
  TEST_SRL( 3,  0xffffffff80000000, 1  );
  TEST_SRL( 4,  0xffffffff80000000, 7  );
  TEST_SRL( 5,  0xffffffff80000000, 14 );
  TEST_SRL( 6,  0xffffffff80000001, 31 );

  TEST_SRL( 7,  0xffffffffffffffff, 0  );
  TEST_SRL( 8,  0xffffffffffffffff, 1  );
  TEST_SRL( 9,  0xffffffffffffffff, 7  );
  TEST_SRL( 10, 0xffffffffffffffff, 14 );
  TEST_SRL( 11, 0xffffffffffffffff, 31 );

  TEST_SRL( 12, 0x0000000021212121, 0  );
  TEST_SRL( 13, 0x0000000021212121, 1  );
  TEST_SRL( 14, 0x0000000021212121, 7  );
  TEST_SRL( 15, 0x0000000021212121, 14 );
  TEST_SRL( 16, 0x0000000021212121, 31 );

  # Verify that shifts only use bottom five bits

  TEST_RR_OP( 17, srl, 0x0000000021212121, 0x0000000021212121, 0xffffffffffffffc0 );
  TEST_RR_OP( 18, srl, 0x0000000010909090, 0x0000000021212121, 0xffffffffffffffc1 );
  TEST_RR_OP( 19, srl, 0x0000000000424242, 0x0000000021212121, 0xffffffffffffffc7 );
  TEST_RR_OP( 20, srl, 0x0000000000008484, 0x0000000021212121, 0xffffffffffffffce );
  TEST_RR_OP( 21, srl, 0x0000000000000000, 0x0000000021212121, 0xffffffffffffffff );

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_RR_SRC1_EQ_DEST( 22, srl, 0x01000000, 0x80000000, 7  );
  TEST_RR_SRC2_EQ_DEST( 23, srl, 0x00020000, 0x80000000, 14 );
  TEST_RR_SRC12_EQ_DEST( 24, srl, 0, 7 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_RR_DEST_BYPASS( 25, 0, srl, 0x01000000, 0x80000000, 7  );
  TEST_RR_DEST_BYPASS( 26, 1, srl, 0x00020000, 0x80000000, 14 );
  TEST_RR_DEST_BYPASS( 27, 2, srl, 0x00000001, 0x80000000, 31 );

  TEST_RR_SRC12_BYPASS( 28, 0, 0, srl, 0x01000000, 0x80000000, 7  );
  TEST_RR_SRC12_BYPASS( 29, 0, 1, srl, 0x00020000, 0x80000000, 14 );
  TEST_RR_SRC12_BYPASS( 30, 0, 2, srl, 0x00000001, 0x80000000, 31 );
  TEST_RR_SRC12_BYPASS( 31, 1, 0, srl, 0x01000000, 0x80000000, 7  );
  TEST_RR_SRC12_BYPASS( 32, 1, 1, srl, 0x00020000, 0x80000000, 14 );
  TEST_RR_SRC12_BYPASS( 33, 2, 0, srl, 0x00000001, 0x80000000, 31 );

  TEST_RR_SRC21_BYPASS( 34, 0, 0, srl, 0x01000000, 0x80000000, 7  );
  TEST_RR_SRC21_BYPASS( 35, 0, 1, srl, 0x00020000, 0x80000000, 14 );
  TEST_RR_SRC21_BYPASS( 36, 0, 2, srl, 0x00000001, 0x80000000, 31 );
  TEST_RR_SRC21_BYPASS( 37, 1, 0, srl, 0x01000000, 0x80000000, 7  );
  TEST_RR_SRC21_BYPASS( 38, 1, 1, srl, 0x00020000, 0x80000000, 14 );
  TEST_RR_SRC21_BYPASS( 39, 2, 0, srl, 0x00000001, 0x80000000, 31 );

  TEST_RR_ZEROSRC1( 40, srl, 0, 15 );
  TEST_RR_ZEROSRC2( 41, srl, 32, 32 );
  TEST_RR_ZEROSRC12( 42, srl, 0 );
  TEST_RR_ZERODEST( 43, srl, 1024, 2048 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 60, 4, -1
RV_COMPLIANCE_DATA_END

