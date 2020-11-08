# See LICENSE for license details.

#*****************************************************************************
# srli.S
#-----------------------------------------------------------------------------
#
# Test srli instruction.
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
  TEST_IMM_OP(n, srli, ((v) & ((1 << (__riscv_xlen-1) << 1) - 1)) >> (a), v, a)

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

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  TEST_IMM_SRC1_EQ_DEST( 17, srli, 0x01000000, 0x80000000, 7 );

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  TEST_IMM_DEST_BYPASS( 18, 0, srli, 0x01000000, 0x80000000, 7  );
  TEST_IMM_DEST_BYPASS( 19, 1, srli, 0x00020000, 0x80000000, 14 );
  TEST_IMM_DEST_BYPASS( 20, 2, srli, 0x00000001, 0x80000001, 31 );

  TEST_IMM_SRC1_BYPASS( 21, 0, srli, 0x01000000, 0x80000000, 7  );
  TEST_IMM_SRC1_BYPASS( 22, 1, srli, 0x00020000, 0x80000000, 14 );
  TEST_IMM_SRC1_BYPASS( 23, 2, srli, 0x00000001, 0x80000001, 31 );

  TEST_IMM_ZEROSRC1( 24, srli, 0, 4 );
  TEST_IMM_ZERODEST( 25, srli, 33, 10 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

