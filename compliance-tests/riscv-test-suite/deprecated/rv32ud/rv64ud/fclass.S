# See LICENSE for license details.

#*****************************************************************************
# fclass.S
#-----------------------------------------------------------------------------
#
# Test fclass.d instruction.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64UF
RV_COMPLIANCE_CODE_BEGIN

#if __riscv_xlen == 32
    # Replace the function with the 32-bit variant defined in test_macros.h
    #undef TEST_FCLASS_D
    #define TEST_FCLASS_D TEST_FCLASS_D32
#endif

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_FCLASS_D( 2, 1 << 0, 0xfff0000000000000 )
  TEST_FCLASS_D( 3, 1 << 1, 0xbff0000000000000 )
  TEST_FCLASS_D( 4, 1 << 2, 0x800fffffffffffff )
  TEST_FCLASS_D( 5, 1 << 3, 0x8000000000000000 )
  TEST_FCLASS_D( 6, 1 << 4, 0x0000000000000000 )
  TEST_FCLASS_D( 7, 1 << 5, 0x000fffffffffffff )
  TEST_FCLASS_D( 8, 1 << 6, 0x3ff0000000000000 )
  TEST_FCLASS_D( 9, 1 << 7, 0x7ff0000000000000 )
  TEST_FCLASS_D(10, 1 << 8, 0x7ff0000000000001 )
  TEST_FCLASS_D(11, 1 << 9, 0x7ff8000000000000 )

  # show finished.
  li  TESTNUM, 12;
  SWSIG (12, TESTNUM);

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

